{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleContexts, TemplateHaskell, LambdaCase #-}

module Cotton.Closure where

import Control.Lens 
import Control.Monad
import qualified Data.Maybe as M

import Cotton.Parser ()
import qualified Cotton.Parser as P
import qualified Cotton.Lexer as L
import qualified Cotton.Type as T

import Control.Monad.State.Strict (State(..))
import qualified Control.Monad.State.Strict as S

import Control.Monad.Writer.Strict (Writer(..))
import qualified Control.Monad.Writer.Strict as W

import Data.Text (Text(..), unpack)
import qualified Data.Text as T

import Data.Monoid ((<>))

import Data.Map.Strict (Map, (!),(!?))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

import qualified Data.List as L

import Debug.Trace

data Expr
    = Fun       { elabel :: Text, eargs  :: [Arg], etype :: T.Type, eterms :: [Term], epos :: L.AlexPosn }
    | Bind      { elabel :: Text, etype :: T.Type, eterms :: [Term],                    epos :: L.AlexPosn }
    deriving Eq

data Term
    = TBind     { label :: Text, type' :: T.Type, term   :: Term,   pos :: L.AlexPosn }
    | If        { cond :: Term,  terms :: [Term],  terms' :: [Term], pos :: L.AlexPosn } -- if式
    | SemiColon { term :: Term, term' :: Term,                      pos :: L.AlexPosn } -- 連結
    | Overwrite { var  :: Text, type' :: T.Type,  term   :: Term,                     pos :: L.AlexPosn } -- 変数上書き
    | Op        { op   :: Text, term   :: Term,   term'  :: Term,   pos :: L.AlexPosn } -- 演算子
    | Call      { var  :: Text, targs  :: [Term],                   pos :: L.AlexPosn } -- Call
    | Str       { text :: Text,                                     pos :: L.AlexPosn } -- 名前
    | TInt      { num  :: Int,                                      pos :: L.AlexPosn } -- 整数
    | Var       { var  :: Text, type' :: T.Type,             posm :: Maybe L.AlexPosn } -- 名前
    deriving Eq

data Arg = Arg { argName :: Text, type'' :: T.Type, apos :: Maybe L.AlexPosn }
    deriving Eq

newtype ImplicitArgs = ImplicitArgs { _implicitArgs :: Map Text (Set Text) }
    deriving (Show, Eq)
makeLenses ''ImplicitArgs

inspectImplicitArgs :: [P.Expr] -> ImplicitArgs
inspectImplicitArgs exprs = ImplicitArgs $ foldr (\expr -> inspectExpr (P.label expr) definedVars expr) M.empty exprs
	where
	definedVars :: Set Text
	definedVars = foldr S.union S.empty [globalVars, preDefined]

	preDefined :: Set Text
	preDefined = S.fromList ["+", "-", "*", "/", "==", "print"]

	globalVars :: Set Text
	globalVars = foldr S.insert S.empty $ map P.label exprs
	
	inspectExpr :: Text
				-> Set Text
	            -> P.Expr 
	            -> Map Text (Set Text)
	            -> Map Text (Set Text)
	inspectExpr block defined expr undefined = case expr of
	    (P.Bind label type' exprs _)      -> 
	        foldr (inspectExpr block defined) undefined exprs
	    (P.Fun  label args type' exprs _) -> 
	        foldr (inspectExpr label $ newDict label args) undefined exprs
	    (P.ETerm term) -> 
	        inspectTerm block defined undefined term 
	    where 
	    newDict l as = definedVars `S.union` foldr (\(P.Arg n t _) dict -> S.insert n dict) (S.singleton l) as
	
	inspectTerm :: Text
				-> Set Text
	            -> Map Text (Set Text) 
	            -> P.Term
	            -> Map Text (Set Text) 
	inspectTerm block defined undefined term = case term of
		(P.TInt num _)			   -> undefined
		(P.Var var _)  			   -> updateDict block var 
		(P.TStr text _)			   -> undefined
		(P.Overwrite var term _)   -> inspectTerm block defined (updateDict block var) term
		(P.Op op term term' _)	   -> let
			undefined'  = inspectTerm block defined (updateDict block op) term
			undefined'' = inspectTerm block defined undefined'			  term
			in undefined''
		(P.Call var targs _)       -> let
			undefined'  = updateDict block var
			undefined'' = foldr (\term undef -> inspectTerm block defined undef term) undefined' targs
			in undefined
		(P.SemiColon term term' _) -> (\u -> inspectTerm block defined u term)
										 $ inspectTerm block defined undefined term
		(P.If cond exprs exprs' _) -> let
			undefined1 = inspectTerm block defined undefined cond
			undefined2 = foldr (inspectExpr block defined) undefined1 exprs
			undefined3 = foldr (inspectExpr block defined) undefined2 exprs
			in undefined3
		where
		updateDict :: Text -> Text -> Map Text (S.Set Text)
		updateDict block var = if var `S.member` defined 
			then undefined
			else M.insertWith S.union block (S.singleton var) undefined 

type Unnest = Writer [Expr]

closure :: T.Env -> [P.Expr] -> [Expr]
closure typeEnv exprs = concat $ mapM (W.execWriter . unnest) exprs
    where
    implicitArgs = _implicitArgs $ inspectImplicitArgs exprs

    pargsToArgs args = flip map args $ \case
        P.Arg n _ p -> Arg n (typeOf n) (Just p)

    typeOf n = T._typeOf typeEnv ! n

    unnest :: P.Expr -> Unnest (Maybe Expr)
    unnest = \case
        (P.Bind l t exprs p) -> do
            terms <- M.catMaybes <$> mapM unnest' exprs
            return . Just $ Bind l (T.Type t) terms p
        (P.Fun l as t exprs p) -> do
            exprs' <- M.catMaybes <$> mapM unnest' exprs
            let impArgs = M.maybe [] S.elems (implicitArgs!?l)
            let args = L.nub $ map (\n -> Arg n (typeOf n) Nothing) impArgs ++ pargsToArgs as
            W.tell [Fun l args (T.Type t) exprs' p]
            return Nothing
        (P.ETerm term) -> error "error" -- グローバルに式が存在？

    unnest' :: P.Expr -> Unnest (Maybe Term)
    unnest' = \case
        (P.Bind l t exprs p) -> do
            terms <- M.catMaybes <$> mapM unnest' exprs
            let (t:ts) = terms
            return . Just $ TBind l (typeOf l) (foldl (\t t' -> SemiColon t t' p) t ts) p
        (P.Fun l as t exprs p) -> do
            terms <- M.catMaybes <$> mapM unnest' exprs
            let impArgs = M.maybe [] S.elems (implicitArgs!?l)
            let args = map (\n -> Arg n (typeOf n) Nothing) impArgs ++ pargsToArgs as
            W.tell [Fun l (pargsToArgs as) (T.Type t) terms p]
            return Nothing
        (P.ETerm term) -> Just <$> unnestTerm term

    unnestTerm :: P.Term -> Unnest Term
    unnestTerm = \case
        (P.TInt num pos)             -> return $ TInt num pos
        (P.Var var pos)              -> return $ Var var (typeOf var) (Just pos)
        (P.TStr text pos)            -> return $ Str text pos
        (P.Overwrite var term pos)   -> Overwrite var (typeOf var) <$> unnestTerm term <*> pure pos
        (P.Op op term term' pos)     -> Op op <$> unnestTerm term <*> unnestTerm term' <*> pure pos
        (P.SemiColon term term' pos) -> SemiColon <$> unnestTerm term <*> unnestTerm term' <*> pure pos
        (P.Call var args pos)        -> do
            args'  <- mapM unnestTerm args
            let impArgs = M.maybe [] S.elems (implicitArgs!?var)
            let args'' = map (\n -> Var n (typeOf n) Nothing) impArgs ++ args'
            Call var <$> pure args'' <*> pure pos
        (P.If cond exprs exprs' pos) -> If <$> unnestTerm cond
                                           <*> (M.catMaybes <$> mapM unnest' exprs)  
                                           <*> (M.catMaybes <$> mapM unnest' exprs') 
                                           <*> pure pos

addIndent = unlines . map ("\t"++) . lines

instance Show Term where
    show (TBind l ty t _p)  = concat ["def ",unpack l,": ",show ty," {\n",addIndent $ show t,"}"]
    show (TInt n _)         = show n
    show (Var l _t _)       = unpack l
    show (Str t _)          = unpack t
    show (Op op t t' _)     = show t ++ " " ++ unpack op ++ " " ++ show t'
    show (Call l  as _)     = unpack l ++ "(" ++ (drop 2 . concat $ map (\a -> ", " ++ show a) as) ++ ")" 
    show (SemiColon t t' _) = show t ++ ";\n" ++ show t'
    show (If c e e' _)      = "if " ++ show c ++ " {\n" ++ (addIndent . unlines $ map show e) 
                            ++ "} else {\n" ++ (addIndent . unlines $ map show e') ++ "}"
    show (Overwrite l ty t _)  = unpack l ++ ": " ++ show ty ++ " <- " ++ show t

instance Show Expr where
    show (Bind l t es _p)   = concat ["def ",unpack l,": ",show t," {\n",addIndent . unlines $ map show es,"}"]
    show (Fun l as t es _p) = concat ["def ",unpack l,"(",drop 2 . concat $ map (\a -> ", " ++ show a) as
                                     ,"): ",show t," {\n",addIndent . unlines $ map show es,"}"]

instance Show Arg where
    show (Arg a t _) = unpack a ++ ": " ++ show t


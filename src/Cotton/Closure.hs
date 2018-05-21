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


data Expr
    = Fun       { elabel :: Text, eargs  :: [Arg], etype :: Text, eterms :: [Term], epos :: L.AlexPosn }
    | Bind      { elabel :: Text, etype :: Text, eterms :: [Term],                    epos :: L.AlexPosn }
    deriving Eq

data Term
    = TBind     { label :: Text, type' :: T.Type, terms :: [Term],  pos :: L.AlexPosn }
    | If        { cond :: Term, terms :: [Term],  terms' :: [Term], pos :: L.AlexPosn } -- if式
    | SemiColon { term :: Term, term' :: Term,                      pos :: L.AlexPosn } -- 連結
    | Overwrite { var  :: Text, term   :: Term,                     pos :: L.AlexPosn } -- 変数上書き
    | Op        { op   :: Text, term   :: Term,   term' :: Term,    pos :: L.AlexPosn } -- 演算子
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

type Defined = Set Text

inspectImplicitArgs :: [P.Expr] -> ImplicitArgs
inspectImplicitArgs exprs  = S.execState (mapM_ (inspectImplicitArgs' "" S.empty) exprs) initState
    where
    -- グローバル変数、関数を環境に追加
    -- TODO: 一段目にETermが来た場合のエラー処理をする
    initState = ImplicitArgs M.empty
    global = foldr (\expr set -> S.insert (P.label expr) set) S.empty exprs

    inspectImplicitArgs' :: Text
                         -> Defined
                         -> P.Expr 
                         -> State ImplicitArgs Defined
    inspectImplicitArgs' blockName defined = \case
        (P.Bind label _ exprs _) -> do
            let defined = S.insert label S.empty
            foldM_ (inspectImplicitArgs' label) defined exprs
            return defined

        (P.Fun label args type' exprs pos) -> do
            let defined = (\defined -> foldr (S.insert . P.argName) defined args)
                               $ S.insert label S.empty
            foldM_ (inspectImplicitArgs' label) defined exprs
            return defined

        (P.ETerm term) -> 
            inspectTerm blockName  defined term

    inspectTerm :: Text 
                -> Defined
                -> P.Term
                -> State ImplicitArgs Defined
    inspectTerm blockName defined = \case
        (P.Var var _)         -> defined <$ ifImplisitArgThenAppend var
        (P.Overwrite var t _) -> inspectTermSub t <* ifImplisitArgThenAppend var
        (P.Op _ t t' _)       -> inspectTermSub t <* inspectTermSub t'
        (P.SemiColon t t' _)  -> inspectTermSub t <* inspectTermSub t'
        (P.Call var ts _)     -> defined <$ mapM_ inspectTermSub ts <* ifImplisitArgThenAppend var
        (P.If t es es' _)     -> defined <$ inspectTermSub t 
                                       <* mapM_ inspectImplicitArgsSub es 
                                       <* mapM_ inspectImplicitArgsSub es'
        t                   -> return defined
        where
        inspectImplicitArgsSub = inspectImplicitArgs' blockName defined
        inspectTermSub = inspectTerm blockName defined
        isDefinedCurBlock label = label `S.member` defined
        ifImplisitArgThenAppend var = 
            unless (isDefinedCurBlock var) (appendImplisitArg blockName var)
        appendImplisitArg blockName varName = 
            implicitArgs %= M.insertWith S.union blockName (S.singleton varName)

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
            return . Just $ Bind l t terms p
        (P.Fun l as t exprs p) -> do
            exprs' <- M.catMaybes <$> mapM unnest' exprs
            let impArgs = M.maybe [] S.elems (implicitArgs!?l)
            let args = map (\n -> Arg n (typeOf n) Nothing) impArgs ++ pargsToArgs as
            W.tell [Fun l args t exprs' p]
            return Nothing
        (P.ETerm term) -> error "error" -- グローバルに式が存在？

    unnest' :: P.Expr -> Unnest (Maybe Term)
    unnest' = \case
        (P.Bind l t exprs p) -> do
            terms <- M.catMaybes <$> mapM unnest' exprs
            return . Just $ TBind l (typeOf l) terms p
        (P.Fun l as t exprs p) -> do
            terms <- M.catMaybes <$> mapM unnest' exprs
            let impArgs = M.maybe [] S.elems (implicitArgs!?l)
            let args = map (\n -> Arg n (typeOf n) Nothing) impArgs ++ pargsToArgs as
            W.tell [Fun l (pargsToArgs as) t terms p]
            return Nothing
        (P.ETerm term) -> Just <$> unnestTerm term

    unnestTerm :: P.Term -> Unnest Term
    unnestTerm = \case
        (P.TInt num pos)             -> return $ TInt num pos
        (P.Var var pos)              -> return $ Var var (typeOf var) (Just pos)
        (P.TStr text pos)            -> return $ Str text pos
        (P.Overwrite var term pos)   -> Overwrite var <$> unnestTerm term <*> pure pos
        (P.Op op term term' pos)     -> Op op <$> unnestTerm term <*> unnestTerm term' <*> pure pos
        (P.SemiColon term term' pos) -> SemiColon <$> unnestTerm term <*> unnestTerm term' <*> pure pos
        (P.Call var args pos)        -> do
            args'  <- mapM unnestTerm args
            let impArgs = M.maybe [] S.elems (implicitArgs!?var)
            let args'' = map (\n -> Var n (typeOf n) Nothing) impArgs ++ args'
            Call var <$> pure args'' <*> pure pos
        (P.If cond exprs exprs' pos) -> If <$> unnestTerm cond
                                           <*> (M.catMaybes <$> mapM unnest' exprs)  
                                           <*> (M.catMaybes <$> mapM unnest' exprs) 
                                           <*> pure pos

addIndent = unlines . map ("\t"++) . lines

instance Show Term where
    show (TBind l t es _p)  = concat ["def ",unpack l,": ",show t," {\n",addIndent . unlines $ map show es,"}"]
    show (TInt n _)         = show n
    show (Var l _t _)       = unpack l
    show (Str t _)          = unpack t
    show (Op op t t' _)     = show t ++ " " ++ unpack op ++ " " ++ show t'
    show (Call l  as _)     = unpack l ++ "(" ++ (drop 2 . concat $ map (\a -> ", " ++ show a) as) ++ ")" 
    show (SemiColon t t' _) = show t ++ ";\n" ++ show t'
    show (If c e e' _)      = "if " ++ show c ++ " {\n" ++ (addIndent . unlines $ map show e) 
                            ++ "} else {\n" ++ (addIndent . unlines $ map show e') ++ "}"
    show (Overwrite l t _)  = unpack l ++ " <- " ++ show t

instance Show Expr where
    show (Bind l t es _p)   = concat ["def ",unpack l,": ",unpack t," {\n",addIndent . unlines $ map show es,"}"]
    show (Fun l as t es _p) = concat ["def ",unpack l,"(",drop 2 . concat $ map (\a -> ", " ++ show a) as
                                     ,"): ",unpack t," {\n",addIndent . unlines $ map show es,"}"]

instance Show Arg where
    show (Arg a t _) = unpack a ++ ": " ++ show t


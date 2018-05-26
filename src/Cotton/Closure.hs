{-|
Module      : Cotton.Closure
Description : closure convert
Copyright   : (c) Sohei Yamaga, 2018
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental
Portability : POSIX

クロージャ変換はネストされた関数定義を展開します。その際内側の自由変数は暗黙の引数として引数に追加されます。

@
def f(): Int {
    def n(): Int = 1;
    def g(): Int {
        n
    }
    g()
}
@

このコードは以下のように展開されます。

@
def g(n: Int): Int {
    n
}
def f(): Int {
    def n(): Int = 1;
    g(n)
}
@

-}

{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase, DeriveFunctor, FlexibleContexts #-}

module Cotton.Closure where

import qualified Cotton.Parser as P
import qualified Cotton.Lexer as L
import qualified Cotton.Type as T

import Data.Text (Text(..), unpack)
import qualified Data.Text as T

import Data.Set (Set, (\\))
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.List as L

import Control.Monad
import qualified Control.Monad.Free as F
import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as W

import qualified Data.Maybe as M

data Stmt
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

data InspectBase cond
    = Define Text Text cond
    | Appear Text Text cond
    deriving Functor

type Inspect = F.Free InspectBase

define l t = F.wrap $ Define l t (return ())
appear l t = F.wrap $ Appear l t (return ())

newtype ImplicitArgs = Imp (Map Text (Set Text))
    deriving Show

runInspect :: Inspect a -> ImplicitArgs
runInspect = Imp . M.map (\\ preDefined) . foldr h M.empty . uncurry (\\) 
    . ((S.fromList . M.catMaybes) >< (S.fromList . M.catMaybes)) . unzip . runInspect'
    where
    h (k,e) = M.insertWith S.union k (S.singleton e)
    (f >< g) (a, b) = (f a, g b)
    preDefined = S.fromList ["+", "-", "*", "/", "==", "print"]
    runInspect' :: Inspect a -> [(Maybe (Text, Text), Maybe (Text, Text))]
    runInspect' = \case
        (F.Free (Define b t cond))  -> (Nothing, Just (b, t)) : runInspect' cond
        (F.Free (Appear b t cond))  -> (Just (b,t), Nothing)  : runInspect' cond
        (F.Pure _)                -> []

inspectImplicitArgs :: [P.Stmt] -> ImplicitArgs
inspectImplicitArgs stmts = runInspect $ mapM_ (inspectStmt "global") stmts
    where
    inspectStmt :: Text -> P.Stmt -> Inspect ()
    inspectStmt block = \case
        (P.Bind l _ stmts _)   -> define block l >> mapM_ (inspectStmt block) stmts
        (P.Fun l as _ stmts _) -> define l l >> mapM_ (define l . P.argName) as
                               >> mapM_ (inspectStmt l) stmts
        (P.ETerm term)         -> inspectTerm block term

    inspectTerm :: Text -> P.Term -> Inspect ()
    inspectTerm block = \case
        (P.Var name _)         -> appear block name
        (P.Overwrite name t _) -> appear block name >> inspectTerm block t
        (P.Op op t t' _)       -> appear block op   >> inspectTerm block t >> inspectTerm block t'
        (P.Call n ts _)        -> mapM_ (inspectTerm block) ts
        (P.SemiColon t t' _)   -> inspectTerm block t >> inspectTerm block t'
        (P.If c ss ss' _)      -> inspectTerm block c
                               >> mapM_ (inspectStmt block) ss
                               >> mapM_ (inspectStmt block) ss'
        _                      -> return ()

type Unnest = Writer [Stmt]

implicitArgsBy :: ImplicitArgs -> Text -> [Text]
implicitArgsBy (Imp d) blockName = S.elems . M.fromMaybe S.empty $ d M.!? blockName

closure :: T.Env -> [P.Stmt] -> [Stmt]
closure typeEnv exprs = concatMap (W.execWriter . unnest) exprs
    where
    implicitArgs = inspectImplicitArgs exprs
    
    pargsToArgs args = flip map args $ \case
        P.Arg n _ p -> Arg n (typeOf n) (Just p)

    typeOf n = T._typeOf typeEnv M.! n

    unnest :: P.Stmt -> Unnest (Maybe Stmt)
    unnest = \case
        (P.Bind l t exprs p) -> do
            terms <- M.catMaybes <$> mapM unnest' exprs
            return . Just $ Bind l (T.Type t) terms p
        (P.Fun l as t exprs p) -> do
            exprs' <- M.catMaybes <$> mapM unnest' exprs
            let impArgs = implicitArgs `implicitArgsBy` l
            let args = map (\n -> Arg n (typeOf n) Nothing) impArgs ++ pargsToArgs as
            W.tell [Fun l args (T.Type t) exprs' p]
            return Nothing
        (P.ETerm term) -> error "error" -- グローバルに式が存在？

    unnest' :: P.Stmt -> Unnest (Maybe Term)
    unnest' = \case
        (P.Bind l t exprs p) -> do
            terms <- M.catMaybes <$> mapM unnest' exprs
            let (t:ts) = terms
            return . Just $ TBind l (typeOf l) (foldl (\t t' -> SemiColon t t' p) t ts) p
        (P.Fun l as t exprs p) -> do
            terms <- M.catMaybes <$> mapM unnest' exprs
            let impArgs = implicitArgs `implicitArgsBy` l
            let args = map (\n -> Arg n (typeOf n) Nothing) impArgs ++ pargsToArgs as
            W.tell [Fun l args (T.Type t) terms p]
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
            let impArgs = implicitArgs `implicitArgsBy` var
            let args'' = map (\n -> Var n (typeOf n) Nothing) impArgs ++ args'
            Call var <$> pure args'' <*> pure pos
        (P.If cond exprs exprs' pos) -> If <$> unnestTerm cond
                                           <*> (M.catMaybes <$> mapM unnest' exprs)  
                                           <*> (M.catMaybes <$> mapM unnest' exprs') 
                                           <*> pure pos

addIndent = unlines . map ("\t"++) . lines

appendImplicitArgs :: T.Env -> ImplicitArgs -> T.Env
appendImplicitArgs (T.Env dict) imp = T.Env $ M.mapWithKey (\name type' -> case type' of
    T.Func args retType -> T.Func (map typeOf (imp `implicitArgsBy` name) ++ args) retType
    t -> t) dict
    where
    typeOf n = dict M.! n

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

instance Show Stmt where
    show (Bind l t es _p)   = concat ["def ",unpack l,": ",show t," {\n",addIndent . unlines $ map show es,"}"]
    show (Fun l as t es _p) = concat ["def ",unpack l,"(",drop 2 . concat $ map (\a -> ", " ++ show a) as
                                     ,"): ",show t," {\n",addIndent . unlines $ map show es,"}"]

instance Show Arg where
    show (Arg a t _) = unpack a ++ ": " ++ show t


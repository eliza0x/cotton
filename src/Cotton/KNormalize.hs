{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleContexts, TemplateHaskell, LambdaCase #-}

module Cotton.KNormalize where

import Control.Lens 
import Control.Monad

import Cotton.Parser (Expr(..), Term(..), Arg(..))
import qualified Cotton.Parser as P

import Control.Monad.State.Strict (State(..))
import qualified Control.Monad.State.Strict as S

import Control.Monad.Writer.Strict (Writer(..))
import qualified Control.Monad.Writer.Strict as W

import Data.Text (Text(..))
import qualified Data.Text as T

import Data.Monoid ((<>))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S


import Debug.Trace

data AlphaEnv = AlphaEnv {
    _prefix :: Text,                  -- ^ プレフィックス
    _rewritedVarName :: Map Text Text -- ^ 変数の書き換え後の名前
    } deriving (Show, Eq)
makeLenses ''AlphaEnv

type Alpha = State AlphaEnv

-- | Alpha変換
alpha :: [Expr] -> [Expr]
alpha exprs = map (\expr -> S.evalState (alpha' expr) initState) exprs
    where 
    initState = AlphaEnv "" global
    -- グローバル変数、関数を環境に追加
    -- TODO: 一段目にETerm来た場合のエラー処理をする
    global = foldr (\expr dic -> M.insert (label expr) (label expr) dic) M.empty exprs
    alpha' :: Expr -> Alpha Expr
    alpha' = \case
        (Bind label' type' expr' pos) -> do
            -- 現在のprefixを変数名に追加
            label <- appendPrefix label'
            -- prefixに現在の変数名を追加
            updatePrefix label
            -- 辞書に更新前の変数名と更新後の変数名の対応を追加
            updateDict label' label
            -- 再起的に更新、環境を引き継ぐ
            expr  <- mapM alpha' expr'
            return Bind{..}
        (Fun label' args' type' expr' pos) -> do
            label <- appendPrefix label'
            updatePrefix label
            -- 引数の変数名を対応した物に変換
            args <- flip mapM args' (\arg -> do
                arg' <- appendPrefix $ argName arg
                updateDict (argName arg) arg'
                return $ Arg arg' (type'' arg) (apos arg)) 
            updateDict label' label
    
            expr  <- mapM alpha' expr'
            return Fun{..}
        (ETerm term) -> ETerm <$> alphaTerm term
        where
        appendPrefix label = uses prefix (\pre -> if T.null pre then pre <> label else pre <> "#" <> label)
        updatePrefix label = prefix .= label
        updateDict key val = rewritedVarName %= M.insert key val
    
    alphaTerm :: Term -> Alpha Term
    alphaTerm = \case
        (Var var pos)              -> flip Var pos <$> findDict var
        (Overwrite var term pos)   -> Overwrite <$> findDict var <*> alphaTerm term <*> pure pos
        (Op op term term' pos)     -> Op op <$> alphaTerm term <*> alphaTerm term'  <*> pure pos
        (Call var args pos)        -> Call <$> findDict var <*> mapM alphaTerm args <*> pure pos
        (SemiColon term term' pos)  -> SemiColon <$> alphaTerm term <*> alphaTerm term' <*> pure pos
        (If cond exprs exprs' pos) -> If <$> alphaTerm cond <*> mapM alpha' exprs <*> mapM alpha' exprs' <*> pure pos
        t                          -> return t
        where
        findDict key = do
            valM <- uses rewritedVarName (M.lookup key)
            case valM of
                Just val -> return val
                Nothing  -> do
                    error $ show key ++ " is not found."
                    error . show . _rewritedVarName <$> S.get  -- ここにmonad failを入れたい

type Unnest = Writer [Expr]
newtype ImplicitArgs = ImplicitArgs { _implicitArgs :: Map Text (Set Text) }
    deriving (Show, Eq)
makeLenses ''ImplicitArgs
type Defined = Set Text

inspectImplicitArgs :: [Expr] -> ImplicitArgs
inspectImplicitArgs exprs  = S.execState (mapM_ (inspectImplicitArgs' "" S.empty) exprs) initState
    where
    -- グローバル変数、関数を環境に追加
    -- TODO: 一段目にETermが来た場合のエラー処理をする
    initState = ImplicitArgs M.empty
    global = foldr (\expr set -> S.insert (label expr) set) S.empty exprs

    inspectImplicitArgs' :: Text
                         -> Defined
                         -> Expr 
                         -> State ImplicitArgs Defined
    inspectImplicitArgs' blockName defined = \case
        (Bind label _ exprs _) -> do
            let defined = S.insert label S.empty
            foldM_ (inspectImplicitArgs' label) defined exprs
            return defined

        (Fun label args type' exprs pos) -> do
            let defined = (\defined -> foldr (S.insert . argName) defined args)
                               $ S.insert label S.empty
            foldM_ (inspectImplicitArgs' label) defined exprs
            return defined

        (ETerm term) -> 
            inspectTerm blockName  defined term

    inspectTerm :: Text 
                -> Defined
                -> Term
                -> State ImplicitArgs Defined
    inspectTerm blockName defined = \case
        (Var var _)         -> defined <$ ifImplisitArgThenAppend var
        (Overwrite var t _) -> inspectTermSub t <* ifImplisitArgThenAppend var
        (Op _ t t' _)       -> inspectTermSub t <* inspectTermSub t'
        (SemiColon t t' _)   -> inspectTermSub t <* inspectTermSub t'
        (Call var ts _)     -> defined <$ mapM_ inspectTermSub ts <* ifImplisitArgThenAppend var
        (If t es es' _)     -> defined <$ inspectTermSub t 
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

unnest :: [Expr] -> [Expr]
unnest = concat . mapM (W.execWriter . unnest')

unnest' :: Expr -> Unnest Expr
unnest' = \case
    (Bind label type' expr pos)     -> undefined
    (Fun label args type' expr pos) -> undefined
    (ETerm term)                    -> undefined

unnestTerm :: Term -> Unnest Term
unnestTerm = \case
    t -> return t

knormalize :: [Expr] -> [Expr]
knormalize = alpha

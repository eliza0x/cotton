{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleContexts, TemplateHaskell, LambdaCase #-}

module Cotton.Alpha where

import Control.Lens 
import Control.Monad
import qualified Data.Maybe as M

import Cotton.Parser ()
import qualified Cotton.Parser as P
import qualified Cotton.Lexer as L

import Control.Monad.State.Strict (State(..))
import qualified Control.Monad.State.Strict as S

import Control.Monad.Writer.Strict (Writer(..))
import qualified Control.Monad.Writer.Strict as W

import Data.Text (Text(..), unpack)
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
alpha :: [P.Stmt] -> [P.Stmt]
alpha exprs = map (\expr -> S.evalState (alpha' expr) initState) exprs
    where 
    initState = AlphaEnv "" global
    -- グローバル変数、関数を環境に追加
    -- TODO: 一段目にETerm来た場合のエラー処理をする
    global = foldr (\expr dic -> M.insert (P.label expr) (P.label expr) dic) M.empty exprs
    alpha' :: P.Stmt -> Alpha P.Stmt
    alpha' = \case
        (P.Bind label' type' expr' pos) -> do
            curPrefix <- use prefix
            -- 現在のprefixを変数名に追加
            label <- appendPrefix label'
            -- prefixに現在の変数名を追加
            updatePrefix label
            -- 辞書に更新前の変数名と更新後の変数名の対応を追加
            updateDict label' label
            -- 再起的に更新、環境を引き継ぐ
            expr  <- mapM alpha' expr'
            -- スコープから出たのでprefixをリセット
            updatePrefix curPrefix
            return P.Bind{..}
        (P.Fun label' args' type' expr' pos) -> do
            curPrefix <- use prefix
            label <- appendPrefix label'
            updatePrefix label
            -- 引数の変数名を対応した物に変換
            args <- flip mapM args' (\arg -> do
                arg' <- appendPrefix $ P.argName arg
                updateDict (P.argName arg) arg'
                return $ P.Arg arg' (P.type'' arg) (P.apos arg)) 
            updateDict label' label
            expr  <- mapM alpha' expr'
            updatePrefix curPrefix
            return P.Fun{..}
        (P.ETerm term) -> P.ETerm <$> alphaTerm term
        where
        infixSeparetar = "_" -- "#"
        appendPrefix label = uses prefix (\pre -> if T.null pre then pre <> label else pre <> "_" <> label)
        updatePrefix label = prefix .= label
        updateDict key val = rewritedVarName %= M.insert key val
    
    alphaTerm :: P.Term -> Alpha P.Term
    alphaTerm = \case
        (P.Var var pos)              -> flip P.Var pos <$> findDict var
        (P.Overwrite var term pos)   -> P.Overwrite <$> findDict var <*> alphaTerm term <*> pure pos
        (P.Op op term term' pos)     -> P.Op op <$> alphaTerm term <*> alphaTerm term'  <*> pure pos
        (P.Call var args pos)        -> P.Call <$> findDict var <*> mapM alphaTerm args <*> pure pos
        (P.SemiColon term term' pos) -> P.SemiColon <$> alphaTerm term <*> alphaTerm term' <*> pure pos
        (P.If cond exprs exprs' pos) -> P.If <$> alphaTerm cond <*> mapM alpha' exprs <*> mapM alpha' exprs' <*> pure pos
        t                          -> return t
        where
        findDict key = do
            valM <- uses rewritedVarName (M.lookup key)
            case valM of
                Just val -> return val
                Nothing  -> do
                    error $ show key ++ " is not found."
                    error . show . _rewritedVarName <$> S.get  -- ここにmonad failを入れたい


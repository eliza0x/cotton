{-|
Module      : Cotton.Alpha
Description : alpha convert
Copyright   : (c) Sohei Yamaga, 2018
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental
Portability : POSIX

alpha変換はソースコード内に現れる変数名をuniqueな物に変換します。

@
def f():Int {
    def n = 123;
    def g():Int {
        def n = 123;
        n
    }
}
@

このコードは以下のように変換されます。

@
def f():Int {
    def f_n = 123;
    def f_g():Int {
        def f_g_n = 123;
        f_g_n
    }
}
@

-}

{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleContexts, TemplateHaskell, LambdaCase #-}

module Cotton.Alpha where

import qualified Cotton.Parser    as P
import qualified Cotton.Type.Type as CT

import Control.Monad
import Data.Monoid ((<>))
import Control.Lens 

import qualified Data.Maybe as M
import Data.Text (Text(..), unpack)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Control.Monad.State.Strict (State(..))
import qualified Control.Monad.State.Strict as S

newtype AlphaEnv = AlphaEnv {
    _rewrittenVarName :: Map Text Text -- ^ 変数の書き換え後の名前
    } deriving (Show, Eq)
makeLenses ''AlphaEnv

type Alpha = State AlphaEnv

-- | Alpha変換
alpha :: [P.Stmt] -> [P.Stmt]
alpha stmts = map (\stmt -> S.evalState (alpha' "" stmt) initState) stmts
    where 
    initState = AlphaEnv (M.fromList [("ref", "ref"),("unref","unref")] `M.union` global)
    -- グローバル変数、関数を環境に追加
    -- TODO: 一段目にETerm来た場合のエラー処理をする
    global = foldr (\stmt dic -> M.insert (P.label stmt) (P.label stmt) dic) M.empty stmts
    alpha' :: Text -> P.Stmt -> Alpha P.Stmt
    alpha' prefix = \case
        (P.Bind label' type' stmt' pos) -> do
            -- prefixを変数名に追加
            let label = prefix <+> label'
            -- 辞書に更新前の変数名と更新後の変数名の対応を追加
            updateDict label' label
            -- 再起的に更新、環境を引き継ぐ
            stmt  <- mapM (alpha' label) stmt'
            -- スコープから出たのでprefixをリセット
            return P.Bind{..}

        (P.Fun label' args' type' stmt' pos) -> do
            let label = prefix <+> label'
            updateDict label' label
            -- 引数の変数名にprefixを追加
            args <- forM args' (\arg -> do
                let arg' = label <+> P.argName arg
                updateDict (P.argName arg) arg'
                return $ P.Arg arg' (P.type'' arg) (P.apos arg)) 
            stmt  <- mapM (alpha' label) stmt'
            return P.Fun{..}

        (P.ETerm term) -> P.ETerm <$> alphaTerm prefix term
        where
        infixSeparator = "_" -- "#"
        t <+> t' = if T.null t then t<>t' else t<>infixSeparator<>t'
        updateDict key val = rewrittenVarName %= M.insert key val
    
    -- 変数名を書き換え後の物で置き換える
    alphaTerm :: Text -> P.Term -> Alpha P.Term
    alphaTerm prefix = \case
        (P.Var var pos)                    -> P.Var <$> findDict var <*> pure pos
        (P.Overwrite var term pos)         -> P.Overwrite <$> findDict var <*> alphaTerm prefix term <*> pure pos
        (P.Op op term term' pos)           -> P.Op op <$> alphaTerm prefix term <*> alphaTerm prefix term'  <*> pure pos
        (P.Call var args pos)              -> P.Call <$> findDict var <*> mapM (alphaTerm prefix) args <*> pure pos
        (P.SemiColon term term' type' pos) -> P.SemiColon <$> alphaTerm prefix term <*> alphaTerm prefix term' <*> pure type' <*> pure pos
        (P.If cond stmts stmts' pos)       -> P.If <$> alphaTerm prefix cond 
                                                   <*> mapM (alpha' prefix) stmts 
                                                   <*> mapM (alpha' prefix) stmts' <*> pure pos
        t -> return t
        where
        findDict key = do
            valM <- uses rewrittenVarName (M.lookup key)
            case valM of
                Just val -> return val
                Nothing  -> do
                    error $ show key ++ " is not found."
                    error . show . _rewrittenVarName <$> S.get  -- ここにmonad failを入れたい


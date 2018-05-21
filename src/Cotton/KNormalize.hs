{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleContexts, TemplateHaskell, LambdaCase #-}

module Cotton.KNormalize where

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
alpha :: [P.Expr] -> [P.Expr]
alpha exprs = map (\expr -> S.evalState (alpha' expr) initState) exprs
    where 
    initState = AlphaEnv "" global
    -- グローバル変数、関数を環境に追加
    -- TODO: 一段目にETerm来た場合のエラー処理をする
    global = foldr (\expr dic -> M.insert (P.label expr) (P.label expr) dic) M.empty exprs
    alpha' :: P.Expr -> Alpha P.Expr
    alpha' = \case
        (P.Bind label' type' expr' pos) -> do
            -- 現在のprefixを変数名に追加
            label <- appendPrefix label'
            -- prefixに現在の変数名を追加
            updatePrefix label
            -- 辞書に更新前の変数名と更新後の変数名の対応を追加
            updateDict label' label
            -- 再起的に更新、環境を引き継ぐ
            expr  <- mapM alpha' expr'
            return P.Bind{..}
        (P.Fun label' args' type' expr' pos) -> do
            label <- appendPrefix label'
            updatePrefix label
            -- 引数の変数名を対応した物に変換
            args <- flip mapM args' (\arg -> do
                arg' <- appendPrefix $ P.argName arg
                updateDict (P.argName arg) arg'
                return $ P.Arg arg' (P.type'' arg) (P.apos arg)) 
            updateDict label' label
    
            expr  <- mapM alpha' expr'
            return P.Fun{..}
        (P.ETerm term) -> P.ETerm <$> alphaTerm term
        where
        appendPrefix label = uses prefix (\pre -> if T.null pre then pre <> label else pre <> "#" <> label)
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

type Unnest = Writer [P.Expr]

data FlatTerm
    = Bind      { label :: Text, type' :: Text, terms :: [FlatTerm],                  pos :: L.AlexPosn }
    | Fun       { label :: Text, args  :: [P.Arg],type' :: Text, terms :: [FlatTerm], pos :: L.AlexPosn }
    | If        { cond :: FlatTerm, terms :: [FlatTerm], terms' :: [FlatTerm],        pos :: L.AlexPosn } -- if式
    | SemiColon { term :: FlatTerm, term' :: FlatTerm,                                pos :: L.AlexPosn } -- 連結
    | Overwrite { var  :: Text, term   :: FlatTerm,                                   pos :: L.AlexPosn } -- 変数上書き
    | Op        { op   :: Text, term   :: FlatTerm, term' :: FlatTerm,                pos :: L.AlexPosn } -- 演算子
    | Call      { var  :: Text, targs  :: [FlatTerm],                                 pos :: L.AlexPosn } -- Call
    | Str       { text :: Text, tpos   :: L.AlexPosn } -- 名前
    | TInt      { num  :: Int,  tpos   :: L.AlexPosn } -- 整数
    | Var       { var  :: Text, tpos   :: L.AlexPosn } -- 名前
    deriving (Show, Eq)

unnest :: [P.Expr] -> [FlatTerm]
unnest exprs = map conv . concat $ mapM (W.execWriter . unnest') exprs
    where
    conv :: P.Expr -> FlatTerm
    conv = \case
        P.Bind{..}     -> let terms = map conv expr in Bind{..}
        P.Fun{..}      -> let
            in Fun label args type' (map conv expr) pos
        (P.ETerm term) -> convTerm term

    convTerm :: P.Term -> FlatTerm
    convTerm = \case
        P.TInt{..}      -> TInt{..}       
        P.Var{..}       -> Var{..}       
        P.TStr{..}      -> Str{..}      
        P.Overwrite{..} -> Overwrite var (convTerm term) tpos
        P.Op{..}        -> Op op (convTerm term) (convTerm term') tpos
        P.Call{..}      -> Call var (map convTerm targs) tpos
        P.SemiColon{..} -> SemiColon (convTerm term) (convTerm term') tpos
        P.If{..}        -> If (convTerm cond) (map conv texprs) (map conv texprs') tpos

    unnest' :: P.Expr -> Unnest (Maybe P.Expr)
    unnest' = \case
        (P.Bind l t exprs p) -> do
            exprs' <- M.catMaybes <$> mapM unnest' exprs
            W.tell [P.Bind l t exprs' p]
            return Nothing
        (P.Fun l as t exprs p) -> do
            exprs' <- M.catMaybes <$> mapM unnest' exprs
            W.tell [P.Fun l as t exprs' p]
            return Nothing
        (P.ETerm term) -> Just . P.ETerm <$> unnestTerm term

    
    unnestTerm :: P.Term -> Unnest P.Term
    unnestTerm = \case
        (P.Overwrite var term pos)   -> P.Overwrite var <$> unnestTerm term <*> pure pos
        (P.Op op term term' pos)     -> P.Op op <$> unnestTerm term <*> unnestTerm term' <*> pure pos
        (P.Call var args pos)        -> P.Call var <$> mapM unnestTerm args <*> pure pos
        (P.SemiColon term term' pos) -> P.SemiColon <$> unnestTerm term <*> unnestTerm term' <*> pure pos
        (P.If cond exprs exprs' pos) -> P.If <$> unnestTerm cond
                                         <*> (M.catMaybes <$> mapM unnest' exprs)  
                                         <*> (M.catMaybes <$> mapM unnest' exprs) 
                                         <*> pure pos
        t                          -> return t        

knormalize :: [P.Expr] -> [P.Expr]
knormalize = alpha

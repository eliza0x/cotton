{-|
Module      : Cotton.KNormalize
Description : K normalize
Copyright   : (c) Sohei Yamaga, 2018
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental
Portability : POSIX

K正規化は式を展開します。

@
def n: Int = 1 * 2 + 3;
@

このコードは以下のように展開されます。

@
def n1: Int = 1;
def n2: Int = 2;
def n3: Int = 3;
def n4: Int = n1 * n2;
def n: Int = n3 + n4;
@

-}

{-# LANGUAGE DataKinds, TypeOperators, LambdaCase, RecordWildCards, TypeApplications, OverloadedStrings, OverloadedLabels, DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints#-}

module Cotton.KNormalize where

import Cotton.Closure (Stmt)
import qualified Cotton.Closure as C
import qualified Cotton.Lexer as L
import qualified Cotton.Type.Type as T
import qualified Cotton.Type as T
import Cotton.Util
import Data.Extensible
import Control.Lens hiding ((:>), op)

import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M

import Data.Maybe (fromMaybe)

import qualified Data.Extensible.Effect as E

import Data.Text (Text, unpack)
import qualified Text.StringRandom as R

import Control.Monad

data Block
    = Fun  { label :: Text, args :: [Val], type' :: T.Type, knorms :: [KNormal], pos :: Maybe L.AlexPosn }
    | Bind { label :: Text,                type' :: T.Type, knorms :: [KNormal], pos :: Maybe L.AlexPosn }
    deriving  Eq

data KNormal
    = Let       {              val1 :: Val, val2 :: Val,              pos :: Maybe L.AlexPosn }
    | Overwrite {              val1 :: Val, val2 :: Val,              pos :: Maybe L.AlexPosn }
    | UnRef     {              val1 :: Val, val2 :: Val,              pos :: Maybe L.AlexPosn }
    | Ref       {              val1 :: Val, val2 :: Val,              pos :: Maybe L.AlexPosn }
    | Op        { op :: Text,  val1 :: Val, val2 :: Val, val3 :: Val, pos :: Maybe L.AlexPosn }
    | Call      { var1 :: Val, fun :: Text, args' :: [Val]          , pos :: Maybe L.AlexPosn }
    | If        { condVar :: Val, retVar :: Val, cond :: [KNormal], then' :: [KNormal], else' :: [KNormal], pos :: Maybe L.AlexPosn }
    deriving Eq

type KNormalize = Eff '[ "knorm" >: E.WriterEff [KNormal]
                       , "dict"  >: E.State (Map Text Text)
                       , "io"    >: IO]

-- | K正規化
knormalize :: T.Env -> [Stmt] -> IO [Block]
knormalize typeEnv = mapM knormalize'
    where
    genVar :: Text -> T.Type -> Maybe L.AlexPosn -> Val
    genVar n t p = #var # (#name @= n <: #type @= t <: #pos @= p <: nil)
    genNum :: Int -> Maybe L.AlexPosn -> Val
    genNum n p   = #int # (#value @= n <:               #pos @= p <: nil)
    genStr :: Text -> Maybe L.AlexPosn -> Val
    genStr t p   = #str # (#text @= t                <: #pos @= p <: nil)
    genReturnVar retType = genVar "_return" retType Nothing
    nullType :: Val
    nullType = #null # Null
    runKnormalize :: KNormalize a -> IO [KNormal]
    runKnormalize = E.retractEff
                  . (`E.evalStateEff` M.empty) 
                  . E.execWriterEff @ "knorm" 

    knormalize' :: Stmt -> IO Block
    knormalize' = \case
        (C.Fun label args type' terms pos) -> 
            Fun label (map (#var #) args) type' <$> knormalizeTerm (genReturnVar type') terms <*> pure (Just pos)
        (C.Bind label type' terms pos)     -> 
            Bind label type'               <$> knormalizeTerm (genReturnVar type') terms <*> pure (Just pos)

    knormalizeTerm :: Val -> [C.Term] -> IO [KNormal]
    knormalizeTerm retVal terms = 
        concat <$> mapM (runKnormalize . knormalizeTerm' retVal) terms
        where
        typeFromEnv n = T._typeOf typeEnv !? n

        knormalizeTerm' :: Val -> C.Term -> KNormalize ()
        knormalizeTerm' retVar = \case
            C.TBind{..} -> knormalizeTerm' (genVar label type' (Just pos)) term 
            C.If{..} -> do
                [n, n'] <- E.liftEff #io $ replicateM 2 uniqueVarName
                let condVar = genVar n (T.Type "Bool") (Just pos)
                let retVar' = genVar n' (typeOf retVar) (Just pos)
                cond' <- E.liftEff #io $ knormalizeTerm condVar [cond]  
                then' <- E.liftEff #io $ knormalizeTerm retVar' terms
                else' <- E.liftEff #io $ knormalizeTerm retVar' terms'
                E.tellEff #knorm [If condVar retVar' cond' then' else' (Just pos)]
                when (retVar /= nullType) $
                    E.tellEff #knorm [Let retVar retVar' (Just pos)]
            C.SemiColon{..} -> do
                knormalizeTerm' nullType term
                knormalizeTerm' retVar term'
            C.Overwrite{..} -> do
                -- 式の返り値を変数名にすることで代入先を決定
                n <- E.liftEff #io uniqueVarName
                let var'  = genVar var type' Nothing
                let var'' = genVar n   (T.unRef type') Nothing
                knormalizeTerm' var'' term
                E.tellEff #knorm [Overwrite var' var'' (Just pos)]
            C.Op{..}  -> do
                let T.Func [t, t'] _ = fromMaybe (error $ "undefined operator: " ++ unpack op) $ typeFromEnv op
                [n, n']  <- E.liftEff #io $ replicateM 2 uniqueVarName
                let (var, var') = (genVar n t Nothing, genVar n' t' Nothing)
                knormalizeTerm' var  term
                knormalizeTerm' var' term'
                E.tellEff #knorm [Op op retVar var var' (Just pos)]
            (C.Call "ref"   [t] p) -> do
                [n, m] <- E.liftEff #io $ replicateM 2 uniqueVarName
                knormalizeTerm' (genVar n (T.unRef $ typeOf retVar) (Just p)) t
                let rd = genVar (nameOf retVar) (typeOf retVar) (Just p)
                    rs = genVar n (T.unRef $ typeOf retVar) (Just p)
                E.tellEff #knorm [Ref rd rs (Just p)]
            (C.Call "unref" [t] p) -> do
                [n, m] <- E.liftEff #io $ replicateM 2 uniqueVarName
                knormalizeTerm' (genVar n (T.Ref $ typeOf retVar) (Just p)) t
                let rd = genVar (nameOf retVar) (typeOf retVar) (Just p)
                    rs = genVar n (T.Ref $ typeOf retVar) (Just p)
                E.tellEff #knorm [UnRef rd rs (Just p)]
            C.Call{..} -> do
                args <- E.liftEff #io $ mapM (const uniqueVarName) [1..length targs] 
                let T.Func types _ = fromMaybe (error $ "undefined arguments: " ++ show args) $ typeFromEnv var
                forM_ (zip3 targs args types) $ \(term, argName, type') ->
                    knormalizeTerm' (genVar argName type' (Just pos)) term
                let valArgs = zipWith (\arg type' -> genVar arg type' (Just pos)) args types
                E.tellEff #knorm [Call retVar var valArgs (Just pos)]
            C.Str{..} -> when (retVar /= nullType) $
                    E.tellEff #knorm [Let retVar (genStr text (Just pos)) (Just pos)]
            C.TInt{..} -> when (retVar /= nullType) $
                    E.tellEff #knorm [Let retVar (genNum num (Just pos)) (Just pos)]
            C.Var{..} -> do
                var' <- fromMaybe var . (!? var) <$> E.getEff #dict
                when (retVar /= nullType) $
                    E.tellEff #knorm [Let retVar (genVar var' type' posm) posm]
            where
            uniqueVarName :: IO Text
            uniqueVarName = R.stringRandomIO "[a-zA-Z][a-zA-Z0-9_]{7}"

instance Show KNormal where
    show (Let v v' _p)       = unpack (showBase v ) ++ " = " ++  unpack (showBase v')
    show (Overwrite v v' _p) = unpack (showBase v ) ++ " <- " ++ unpack (showBase v')
    show (UnRef v v' _p)     = unpack (showBase v ) ++ " = unref(" ++ unpack (showBase v') ++ ")"
    show (Ref v v' _p)       = unpack (showBase v ) ++ " = ref(" ++ unpack (showBase v') ++ ")"
    show (Op op v1 v2 v3 _)  = unpack (showBase v1) ++ " = " ++ unpack (showBase v2)  ++ " " ++ unpack op ++ " " ++ unpack (showBase v3)
    show (Call v1 l as _)    = unpack (showBase v1) ++ " = " ++ unpack l ++ 
                                                       "(" ++ (drop 2 . concat $ map (\a -> ", " ++ unpack (showBase a)) as) ++ ")" 
    show (If cv _ c e e' _) = unlines (map show c) ++ "if "++show cv++" {\n" ++ (addIndent . unlines $ map show e) 
                            ++ "} else {\n" ++ (addIndent . unlines $ map show e') ++ "}"

instance Show Block where
    show (Fun l as t ks _p) = concat ["def ",unpack l,"(",drop 2 . concat $ map (\a -> ", " ++ unpack (showBase a)) as
                                     ,"): ",show t," {\n",addIndent . unlines $ map show ks,"}"]
    show (Bind l t es _p)   = concat ["def ",unpack l,": ",show t," {\n",addIndent . unlines $ map show es,"}"]


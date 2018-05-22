{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, PolyKinds, FlexibleContexts, LambdaCase, RecordWildCards, TypeApplications, OverloadedStrings, OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints#-}

module Cotton.KNormalize where

import Cotton.Closure (Expr, Term)
import qualified Cotton.Closure as C
import qualified Cotton.Lexer  as L
import qualified Cotton.Type   as T

import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M

import Data.Maybe (fromMaybe)

import Data.Extensible.Internal 
import Data.Extensible
import qualified Data.Extensible.Effect as E

import qualified Control.Monad.State.Class as S

import Data.Text (Text, unpack)
import qualified Text.StringRandom as R

import Control.Monad
import Debug.Trace

data Block
    = Fun  { label :: Text, args :: [Val], btype :: T.Type, knorms :: [KNormal], bpos :: Maybe L.AlexPosn }
    | Bind { label :: Text,                btype :: T.Type, knorms :: [KNormal], bpos :: Maybe L.AlexPosn }
    deriving  Eq

data KNormal
    = Let  {              val1 :: Val, val2 :: Val,              pos :: Maybe L.AlexPosn }
    | Op   { op :: Text,  val1 :: Val, val2 :: Val, val3 :: Val, pos :: Maybe L.AlexPosn }
    | Call { var1 :: Val, fun :: Text, args' :: [Val]               , pos :: Maybe L.AlexPosn }
    | If   { condVar :: Val, retVar :: Val, cond :: [KNormal], then' :: [KNormal], else' :: [KNormal], pos :: Maybe L.AlexPosn }
    deriving Eq

data Val
    = Var     { name :: Text, type' :: T.Type, vpos :: Maybe L.AlexPosn }
    | NullVar
    | Num     { num  :: Int,                   vpos :: Maybe L.AlexPosn }
    | Str     { text :: Text,                  vpos :: Maybe L.AlexPosn }
    deriving Eq

type KNormalize = Eff '[ "knorm" >: E.WriterEff [KNormal]
                       , "dict"  >: E.State (Map Text Text)
                       , "io"    >: IO]

-- | K正規化
knormalize :: T.Env -> [Expr] -> IO [Block]
knormalize typeEnv = mapM knormalize'
    where
    genReturnVar retType = Var "#return" retType Nothing

    runKnormalize :: KNormalize a -> IO [KNormal]
    runKnormalize = E.retractEff
                  . (`E.evalStateEff` M.empty) 
                  . E.execWriterEff @ "knorm" 

    knormalize' :: Expr -> IO Block
    knormalize' = \case
        (C.Fun label args type' terms pos) -> Fun label (map arg2Val args) type' <$> knormalizeTerm (genReturnVar type') terms <*> pure (Just pos)
        (C.Bind label type' terms pos)     -> Bind label type'                   <$> knormalizeTerm (genReturnVar type') terms <*> pure (Just pos)
        where
        arg2Val C.Arg{..} = Var argName type'' apos

    knormalizeTerm :: Val -> [C.Term] -> IO [KNormal]
    knormalizeTerm retVal terms = 
        concat <$> mapM (runKnormalize . knormalizeTerm' retVal) terms
        where
        typeOf = T._typeOf typeEnv
        valType = \case
            Var{..} -> type'
            NullVar -> T.Bottom
            Num{..} -> T.Type "Int"
            Str{..} -> T.Type "String" 

        knormalizeTerm' :: Val -> C.Term -> KNormalize ()
        knormalizeTerm' retVar = \case
            C.TBind{..} -> knormalizeTerm' (Var label type' (Just pos)) term 
            C.If{..} -> do
                [n, n'] <- E.liftEff #io $ replicateM 2 uniqueVarName
                let condVar = Var n (T.Type "Bool") (Just pos)
                let retVar' = Var n' (valType retVar) (Just pos)
                cond' <- E.liftEff #io $ knormalizeTerm condVar [cond]  
                then' <- E.liftEff #io $ knormalizeTerm retVar' terms
                else' <- E.liftEff #io $ knormalizeTerm retVar' terms'
                E.tellEff #knorm [If condVar retVar' cond' then' else' (Just pos)]
                when (retVar /= NullVar) $
                    E.tellEff #knorm [Let retVar retVar' (Just pos)]
            C.SemiColon{..} -> do
                knormalizeTerm' NullVar term
                knormalizeTerm' retVar term'
            C.Overwrite{..} -> do
                -- 式の返り値を変数名にすることで代入先を決定
                let var' = Var var type' Nothing
                knormalizeTerm' var' term
                -- when (retVar /= NullVar) $
                --     E.tellEff #knorm [Let retVar var' (Just pos)]
            C.Op{..}  -> do
                let T.Func [t, t'] _ = fromMaybe (error $ "undefined operator: " ++ unpack op) $ typeOf !? op

                [n, n']  <- E.liftEff #io $ replicateM 2 uniqueVarName
                let (var, var') = (Var n t Nothing, Var n' t' Nothing)

                knormalizeTerm' var  term
                knormalizeTerm' var' term'
                E.tellEff #knorm [Op op retVar var var' (Just pos)]
            C.Call{..} -> do
                args <- E.liftEff #io $ mapM (const uniqueVarName) [1..length targs] 
                let T.Func types _ = fromMaybe (error $ "undefined function name: " ++ unpack var) $ typeOf !? var
                forM_ (zip3 targs args types) $ \(term, argName, type') ->
                    knormalizeTerm' (Var argName type' (Just pos)) term
                let valArgs = map (\(arg, type') -> Var arg type' (Just pos)) $ zip args types
                E.tellEff #knorm [Call retVar var valArgs (Just pos)]
            C.Str{..} -> when (retVar /= NullVar) $
                    E.tellEff #knorm [Let retVar (Str text (Just pos)) (Just pos)]
            C.TInt{..} -> when (retVar /= NullVar) $
                    E.tellEff #knorm [Let retVar (Num num (Just pos)) (Just pos)]
            C.Var{..} -> do
                var' <- fromMaybe var . (!? var) <$> E.getEff #dict
                when (retVar /= NullVar) $
                    E.tellEff #knorm [Let retVar (Var var' type' posm) posm]
            where
            uniqueVarName :: IO Text
            uniqueVarName = R.stringRandomIO "[a-zA-Z0-9_]{8}"

instance Show KNormal where
    show (Let v v' _p)      = show v  ++ " = " ++ show v'
    show (Op op v1 v2 v3 _) = show v1 ++ " = " ++ show v2  ++ " " ++ unpack op ++ " " ++ show v3
    show (Call v1 l as _)   = show v1 ++ " = " ++ unpack l ++ "(" ++ (drop 2 . concat $ map (\a -> ", " ++ show a) as) ++ ")" 
    show (If cv rv c e e' _) = unlines (map show c) ++ "if #cond {\n" ++ (addIndent . unlines $ map show e) 
                            ++ "} else {\n" ++ (addIndent . unlines $ map show e') ++ "}"

instance Show Block where
    show (Fun l as t ks _p) = concat ["def ",unpack l,"(",drop 2 . concat $ map (\a -> ", " ++ show a) as
                                     ,"): ",show t," {\n",addIndent . unlines $ map show ks,"}"]
    show (Bind l t es _p)   = concat ["def ",unpack l,": ",show t," {\n",addIndent . unlines $ map show es,"}"]

instance Show Val where
    show (Var n t _) = unpack n
    show NullVar     = "_"
    show (Num n _)   = show n
    show (Str t _)   = show t
    -- show (Var n t _) = unpack n ++ ": " ++ show t

addIndent = unlines . map ("\t"++) . lines

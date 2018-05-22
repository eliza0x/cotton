{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Cotton.Emit where

import Data.Text (Text)
import qualified Data.Map.Strict as M

import qualified Cotton.KNormalize as K

data LLVM_IR 
    = Fun  { label :: Text, retType :: Type, args :: Var, insts :: [Instraction] }
    | Bind { label :: Text, retType :: Type, ival :: Val }
    deriving (Eq, Show)	

data Instraction 
    = Alloca { rd :: Var,             type' :: Type }
    | Store  { rd :: Var, val :: Val, type' :: Type }
    | Load   { rd :: Var, rs :: Var,  type' :: Type }
    | Add    { rd :: Var, rs :: Var, rt :: Var }
    | Sub    { rd :: Var, rs :: Var, rt :: Var }
    | Mul    { rd :: Var, rs :: Var, rt :: Var }
    | Div    { rd :: Var, rs :: Var, rt :: Var }
    | Eqi    { rd :: Var, rs :: Var, rt :: Var }
    | Br     { rd :: Var, rs :: Var, rt :: Var }
    | Label  { id :: Int }
    deriving (Eq, Show)

data Type = Type
    deriving (Eq, Show)

data Var 
    = IVar Int       -- 数値
    | TVar Text Type -- Varに変換前の変数
    | GVar Text Type -- グローバル変数
    | Var Int        -- 変数、レジスタ番号を管理
    deriving (Eq, Show)

data Val
    = VInt Int
    deriving (Eq, Show)

emit :: [K.Block] -> [LLVM_IR]
emit blocks = undefined

toSSA :: [K.KNormal] -> [K.KNormal]
toSSA = undefined

block2LLVM_IR :: K.Block -> (LLVM_IR, [Instraction])
block2LLVM_IR = case
    K.Fun{..}  -> (Fun  label btype (map val2Var args) (kNormal2Instraction 0 (args) knorms), [])
    K.Bind{..} -> (Bind label btype (VInt 0), kNormal2Instraction 1 [] knorms)

kNormal2Instraction :: Int -> M.Map Text Int -> [K.KNormal] -> [Instraction]
kNormal2Instraction nextVarNum dict (knorm:ks) = undefined
     
{-
kNormal2Instraction' :: [K.Var] -> [K.KNormal] -> [Instraction]
kNormal2Instraction' i (knorm:ks) = case knorm of
    Let  {..} -> [Alloca 
    Op   {..} -> 
    Call {..} -> 
    If   {..} -> 
-}
    
val2Var :: K.Val -> Var
val2Var = undefined

{-
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
-}


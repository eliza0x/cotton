{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings, FlexibleContexts #-}

module Cotton.Emit where

import Data.Text (Text, unpack)
import Data.Map.Strict ((!), (!?))
import qualified Data.Map.Strict as M

import qualified Control.Monad.State.Strict as S

import Data.Maybe

import qualified Cotton.KNormalize as K
import qualified Cotton.Type as T

data LLVM_IR 
    = Fun  { label :: Text, retType :: T.Type, args :: [Var], insts :: [Instraction] }
    | Bind { label :: Text, retType :: T.Type, ival :: Var }
    deriving Eq

instance Show LLVM_IR where
    show (Fun l t as is) = "define "++show t++" @"++unpack l++"("++drop 2 (concatMap ((", "++) . show) as)++") {"
                        ++ addIndent (concatMap (("\n"++) . show) is) ++ "}\n"
    show (Bind l t v)    = "@"++unpack l++" = global "++show t++" "++show v++" align 4\n"

data Instraction 
    = Alloca { rd :: Var,            type' :: T.Type }
    | Store  { rd :: Var, rs :: Var, type' :: T.Type }
    | Load   { rd :: Var, rs :: Var, type' :: T.Type }
    | Call   { rd :: Var, label' :: Text, type' :: T.Type, args' :: [Var] }
    | Add    { rd :: Var, rs :: Var, rt :: Var }
    | Sub    { rd :: Var, rs :: Var, rt :: Var }
    | Mul    { rd :: Var, rs :: Var, rt :: Var }
    | Div    { rd :: Var, rs :: Var, rt :: Var }
    | Eqi    { rd :: Var, rs :: Var, rt :: Var }
    | CBr    { cond :: Var, thenLabel :: Text, elseLabel :: Text }
    | Br     { label' :: Text }
    | Label  { label' :: Text }
    deriving Eq

instance Show Instraction where
    show = \case
        (Alloca rd        type')        -> "%"++show rd ++ " = alloca "++show type'++", align 4"
        (Store  rd rs     type')        -> undefined
        (Load   rd rs     type')        -> undefined
        (Call   rd label' type' args') -> undefined
        (Add    rd rs rt)               -> undefined
        (Sub    rd rs rt)               -> undefined
        (Mul    rd rs rt)               -> undefined
        (Div    rd rs rt)               -> undefined
        (Eqi    rd rs rt)               -> undefined
        (CBr cond then' else')          -> undefined
        (Br     label')                 -> undefined
        (Label  label')                 -> undefined

data Var 
    = VInt Int         -- 数値
    | VBool Bool
    | VStr Text
    | TVar Text T.Type
    | GVar Text T.Type -- グローバル変数
    | Var  Int  T.Type -- 変数、レジスタ番号を管理
    | Null             -- 書き込み専用
    deriving Eq

instance Show Var where
    show (VInt n)       = show n
    show (VBool True)   = show 1
    show (VBool False)  = show 0
    show (VStr t)       = show t
    show (TVar t type') = unpack t
    show (GVar t type') = unpack t -- グローバル変数
    show (Var  n type') = show n   -- 変数、レジスタ番号を管理
    show Null           = "null"   -- 書き込み専用
 

addIndent = unlines . map ("\t"++) . lines

emit :: [K.Block] -> [LLVM_IR]
emit blocks = let
    (ir, insts) = unzip $ map block2LLVM_IR blocks
    in Fun "init" (T.Type "Unit") [] (concat insts) : ir

block2LLVM_IR :: K.Block -> (LLVM_IR, [Instraction])
block2LLVM_IR = \case
    K.Fun{..}  -> (Fun label btype (map val2Var args) 
        (kNormal2Instraction args knorms), [])
    K.Bind{..} -> (Bind label btype (initVal btype), kNormal2Instraction [] knorms)
    where
    genDict args = foldr (\(i, arg) dict -> M.insert (K.name arg) i dict) M.empty $ zip [0..] args
    initVal = \case
        T.Type "Int"    -> VInt 0
        T.Type "Bool"   -> VBool False
        T.Type "String" -> VStr ""

type InstM = S.State (M.Map Text Int)

kNormal2Instraction :: [K.Val] ->  [K.KNormal] -> [Instraction]
kNormal2Instraction args knorms = header ++ concatMap (\knorm -> S.evalState (kNormal2Instraction' knorm) initState) knorms
    where
    initState :: M.Map Text Int
    initState = foldr (\(arg, id) dict -> M.insert (K.name arg) id dict) M.empty (zip args [1..])


    isDefined :: K.Val -> InstM Bool
    isDefined val = isJust . (!? K.name val) <$> S.get

    allocVar :: K.Val -> InstM [Instraction]
    allocVar val1 = do
        b <- isDefined val1
        if b then do
            m <- maximum <$> S.get
            S.modify $ M.insert (K.name val1) (m+1)
            return [Alloca (val2Var val1) (K.type' val1)] 
            else return []

    kNormal2Instraction' :: K.KNormal -> InstM [Instraction]
    kNormal2Instraction' knorm = case knorm of
        (K.Op "+"  v1 v2 v3 _) -> (++) <$> allocVar v1 <*> pure [Add (val2Var v1) (val2Var v2) (val2Var v3)]
        (K.Op "-"  v1 v2 v3 _) -> (++) <$> allocVar v1 <*> pure [Sub (val2Var v1) (val2Var v2) (val2Var v3)]
        (K.Op "*"  v1 v2 v3 _) -> (++) <$> allocVar v1 <*> pure [Mul (val2Var v1) (val2Var v2) (val2Var v3)]
        (K.Op "/"  v1 v2 v3 _) -> (++) <$> allocVar v1 <*> pure [Div (val2Var v1) (val2Var v2) (val2Var v3)]
        (K.Op "==" v1 v2 v3 _) -> (++) <$> allocVar v1 <*> pure [Eqi (val2Var v1) (val2Var v2) (val2Var v3)]
        (K.Op fun  v1 v2 v3 _) -> (++) <$> allocVar v1 <*> pure [Call(val2Var v1) fun (K.type' v1) (map val2Var [v2,v3])]
        K.Let{..}              -> (++) <$> allocVar val1 <*> pure [Store (val2Var val1) (val2Var val2) (K.type' val1)]
        K.Call{..}             -> (++) <$> allocVar var1 <*> pure [Call (val2Var var1) fun (K.type' var1) (map val2Var args)]
        (K.If condVar retVar cond then' else' _) -> do
            condInsts <- concat <$> mapM kNormal2Instraction' cond
            thenInsts <- concat <$> mapM kNormal2Instraction' then'
            elseInsts <- concat <$> mapM kNormal2Instraction' else'
            return $ condInsts ++ [CBr (val2Var condVar) "then" "else"] ++ 
                     [Label "then"] ++ thenInsts ++ [Br "continue"] ++
                     [Label "else"] ++ elseInsts ++ [Br "continue"] ++
                     [Label "continue"]
            
    header :: [Instraction]
    header = flip concatMap args $ \arg -> case arg of
        K.Var{..} -> [Alloca (TVar name type') type', Store (TVar name type') (val2Var arg) type']
        v         -> error $ show v
    
val2Var :: K.Val -> Var
val2Var = \case
    K.Var{..} -> TVar name type'
    K.Num{..} -> VInt num
    K.Str{..} -> VStr text
    K.NullVar -> error "NullVar"



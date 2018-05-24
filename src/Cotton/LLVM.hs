{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings, FlexibleContexts, DeriveFunctor #-}

module Cotton.LLVM where

import Data.Maybe
import Control.Monad
import Data.Text (Text, unpack)
import Data.Map.Strict ((!), (!?))
import qualified Data.Set as Se
import qualified Data.Map.Strict as M

import qualified Text.StringRandom as R

import qualified Control.Monad.State.Strict as S
import qualified Control.Monad.IO.Class as S

import qualified Cotton.KNormalize as K
import qualified Cotton.Type as T

import Control.Monad.Free
import Control.Monad
import Data.Monoid
import Data.Functor

data LLVM_IR 
    = Fun  { label :: Text, retType :: T.Type, args :: [Reg], insts :: [Instraction] }
    | Bind { label :: Text, retType :: T.Type, ival :: Reg }
    deriving Eq

instance Show LLVM_IR where
    show (Bind l t v)    = "@"++unpack l++" = global "++show t++" "++show v++" align 4\n"
    show (Fun l t as is) = 
        "define "++show t++" @"++unpack l++
        "("++drop 2 (concatMap (\(Reg n t) -> ", "++show t++" %"++unpack n) as)++") "++ 
        " {\n"++ (concatMap (\i -> indent i ++ show i ++"\n") is) ++ 
        "\tret "++show t++(if t == T.Type "Unit" then "" else " %_return")++
        "\n}\n"
        where    
        indent Label{..}  = ""
        indent _          = "\t"

data Instraction 
    = Alloca { rd   :: Ref,            type' :: T.Type }
    | Store  { rd   :: Ref, rs  :: Reg, type' :: T.Type }
    | Load   { rd'  :: Reg, rs' :: Ref, type' :: T.Type }
    | Call   { label' :: Text,  type' :: T.Type, rd' :: Reg, args' :: [Reg] }
    | Add    { rd'  :: Reg, rs  :: Reg, rt :: Reg }
    | Sub    { rd'  :: Reg, rs  :: Reg, rt :: Reg }
    | Mul    { rd'  :: Reg, rs  :: Reg, rt :: Reg }
    | Div    { rd'  :: Reg, rs  :: Reg, rt :: Reg }
    | Eqi    { rd'  :: Reg, rs  :: Reg, rt :: Reg }
    | CBr    { cond :: Reg, thenLabel :: Text, elseLabel :: Text }
    | Br     { label' :: Text }
    | Label  { label' :: Text }
    deriving Eq

instance Show Instraction where
    show = \case
        (Alloca rd    type') -> show rd ++ " = alloca "++show type'++", align 4"
        (Store  rd rs type') -> "store "++show type'++" "++show rs++", "++show type'++"* "++show rd++", align 4"
        (Load   rd rs type') -> show rd ++ " = load "++show type'++", "++show type'++"* "++show rs++", align 4"
        (Add    rd rs rt)    -> show rd ++ " = add nsw i32 "++show rs++", "++show rt
        (Sub    rd rs rt)    -> show rd ++ " = sub nsw i32 "++show rs++", "++show rt
        (Mul    rd rs rt)    -> show rd ++ " = mul nsw i32 "++show rs++", "++show rt
        (Div    rd rs rt)    -> show rd ++ " = div nsw i32 "++show rs++", "++show rt
        (Eqi    rd rs rt)    -> show rd ++ " = icmp eq i32 "++show rs++", "++show rt
        (CBr cond t e)       -> "br i1 "++show cond++", label %"++unpack t++", label %"++unpack e
        (Br     label')      -> "br label %"++unpack label'
        (Label  label')      -> "\n"++unpack label'++":"
        (Call lbl type' rd args') -> show rd++" = call "++show type'++" @"++unpack lbl++
                                    "("++drop 2 (concatMap (\(Reg n t) -> ", "++show t++" %"++unpack n) args')++") "

data Ref = Ref Text T.Type
    deriving Eq

instance Show Ref where
    show (Ref t _) = "%" ++ unpack t

data Reg 
    = I32 Int         -- 数値
    | VBool Bool
    | Str Text
    | Reg Text T.Type
    | GReg Text T.Type -- グローバル変数
    | Null             -- 書き込み専用
    deriving Eq

instance Show Reg where
    show (I32 n)    = show n
    show (Str t)    = show t
    show (Reg t _)  = "%"++unpack t
    show (GReg t _) = "@"++unpack t -- グローバル変数
    show Null       = "null"   -- 書き込み専用
 
knorm2llvmir :: [K.Block] -> IO [LLVM_IR]
knorm2llvmir blocks = do
    (ir, insts) <- unzip <$> mapM block2LLVM_IR blocks
    return $ Fun "init" (T.Type "Unit") [] (concat insts) : ir

block2LLVM_IR :: K.Block -> IO (LLVM_IR, [Instraction])
block2LLVM_IR = \case
    K.Fun{..}  -> do
        llvmir <- kNormal2Instraction args knorms
        let llvmir' = filter isAlloca llvmir ++ filter (not . isAlloca) llvmir
        return (Fun label btype (map val2Reg args) llvmir', [])
    K.Bind{..} -> do 
        llvmir <- kNormal2Instraction [] knorms
        let llvmir' = filter isAlloca llvmir ++ filter (not . isAlloca) llvmir
        return (Bind label btype (initVal btype), llvmir')
    where
    isAlloca Alloca{..} = True
    isAlloca _          = False
    genDict args = foldr (\(i, arg) dict -> M.insert (K.name arg) i dict) M.empty $ zip [0..] args
    initVal = \case
        T.Type "I32"    -> I32 0
        T.Type "Bool"   -> VBool False
        T.Type "String" -> Str ""

type InstM = S.StateT (Se.Set Text) IO

kNormal2Instraction :: [K.Val] ->  [K.KNormal] -> IO [Instraction]
kNormal2Instraction blockArgs knorms =
    concat <$> S.evalStateT (mapM (\knorm -> (kNormal2Instraction' knorm)) knorms) initState 
    where
    initState :: Se.Set Text 
    initState = foldr (\arg dict -> Se.insert (K.name arg) dict) (Se.singleton "_return") blockArgs

    allocRef :: K.Val -> InstM [Instraction]
    allocRef (K.Var name type' _) = do
        set <- S.get
        S.put $ Se.insert name set
        if name `Se.member` set then return []
                                else return [Alloca (Ref name type') type']  
                
    allocRef _ = return []

    kNormal2Instraction' :: K.KNormal -> InstM [Instraction]
    kNormal2Instraction' knorm = case knorm of
        (K.Op "+"  r1 r2 r3 _) -> genOpInst Add r1 r2 r3
        (K.Op "-"  r1 r2 r3 _) -> genOpInst Sub r1 r2 r3
        (K.Op "*"  r1 r2 r3 _) -> genOpInst Mul r1 r2 r3
        (K.Op "/"  r1 r2 r3 _) -> genOpInst Div r1 r2 r3
        (K.Op "==" r1 r2 r3 _) -> genOpInst Eqi r1 r2 r3
        (K.Op fun  r1 r2 r3 _) -> genCallInst fun r1 [r2,r3]
        (K.Call r1 fun args _) -> genCallInst fun r1 args
        K.Let{..}              -> do
            allocInst <- allocRef val1
            case (K.name val1, isArg val2, val2) of
                -- 返り値は値であり、引数は値であるため
                ("_return", True, _) -> do
                    refName <- uniqueText
                    allocInst' <- allocRef (K.Var refName (typeOf val2) Nothing)
                    let storeInst = Store (Ref refName (K.type' val2)) (val2Reg val2) (typeOf val2)
                    let loadInst = Load (val2Reg val1) (Ref refName (K.type' val2)) (typeOf val1)
                    return $ allocInst ++ allocInst' ++ [storeInst, loadInst]
               
                -- 返り値は値のため
                ("_return", _, K.Var{..}) -> do
                    let loadInst = Load (val2Reg val1) (val2Ref val2) (typeOf val2)
                    return $ allocInst ++ [loadInst]

                ("_return", _, _) -> do
                    refName <- uniqueText
                    allocInst' <- allocRef (K.Var refName (typeOf val2) Nothing)
                    let storeInst = Store (Ref refName (K.type' val2)) (val2Reg val2) (typeOf val2)
                    let loadInst = Load (val2Reg val1) (Ref refName (K.type' val2)) (typeOf val1)
                    return $ allocInst ++ allocInst' ++ [storeInst, loadInst]

                -- 引数は参照ではなく値で与えられるため 
                (_, True, K.Var{..}) -> do
                    regName <- uniqueText
                    let storeInst = Store (val2Ref val1) (val2Reg val2) (typeOf val1)
                    return $ allocInst ++ [storeInst]

                (_, _, K.Var{..}) -> do
                    regName <- uniqueText
                    let loadInst = Load (Reg regName (typeOf val2)) (val2Ref val2) (typeOf val2)
                    let storeInst = Store (val2Ref val1) (Reg regName (typeOf val2)) (typeOf val1)
                    return $ allocInst ++ [loadInst, storeInst]
                (_, _, _) -> do
                    let storeInst = Store (val2Ref val1) (val2Reg val2) (typeOf val2)
                    return $ allocInst ++ [storeInst]
        (K.If condReg retReg cond then' else' _) -> do
            [t,e,c] <- replicateM 3 uniqueText
            condInsts <- concat <$> mapM kNormal2Instraction' cond
            thenInsts <- concat <$> mapM kNormal2Instraction' then'
            elseInsts <- concat <$> mapM kNormal2Instraction' else'

            crName <- uniqueText
            let loadInst = Load (Reg crName $ typeOf condReg) (val2Ref condReg) (typeOf condReg)
            return $ 
                     condInsts ++ [loadInst, CBr (Reg crName $ typeOf condReg) ("then_"<>t) ("else_"<>e)] ++ 
                     [Label ("then_"<>t)] ++ thenInsts ++ [Br ("continue_"<>c)] ++
                     [Label ("else_"<>e)] ++ elseInsts ++ [Br ("continue_"<>c)] ++
                     [Label ("continue_"<>c)]
        where
        isArg (K.Var name _ _) = name `elem` map K.name blockArgs
        isArg _                = False

        typeOf = \case
            K.Var{..} -> type'
            K.NullVar -> T.Bottom
            K.Num{..} -> T.Type "I32"
            K.Str{..} -> T.Type "String"

        uniqueText :: InstM Text
        uniqueText = S.liftIO $ R.stringRandomIO "[a-zA-Z][a-zA-Z0-9_]{7}"


        genCallInst funName (K.Var "_return" type' _) args = do
            let rd = K.Var "_return" type' Nothing
            names <- replicateM (length args) uniqueText
            let (args', loadsM) = unzip $ flip map (zip names args) (\(newName, arg) -> case arg of
                    K.Var{..} -> (Reg newName type', Just $ Load (Reg newName type') (Ref name type') type') 
                    v         -> (val2Reg v, Nothing))
            allocInst <- allocRef rd
            let callInst = Call funName (typeOf rd) (val2Reg rd) args'
            return $ allocInst ++ catMaybes loadsM ++ [callInst]

        genCallInst funName rd args = do
            names <- replicateM (length args) uniqueText
            let (args', loadsM) = unzip $ flip map (zip names args) (\(newName, arg) -> case arg of
                    K.Var{..} -> (Reg newName type', Just $ Load (Reg newName type') (Ref name type') type') 
                    v         -> (val2Reg v, Nothing))
            regName <- uniqueText
            allocInst <- allocRef rd
            let callInst = Call funName (typeOf rd) (Reg regName (typeOf rd)) args'
            let storeInst = Store (val2Ref rd) (Reg regName (typeOf rd)) (typeOf rd)
            return $ allocInst ++ catMaybes loadsM ++ [callInst, storeInst]

        genOpInst op r1 r2 r3 = do
            names <- replicateM 2 uniqueText
            let ([reg2, reg3], loadsM) = unzip $ flip map (zip names [r2,r3]) (\(newName, arg) -> case arg of
                    K.Var{..} -> (Reg newName type', Just $ Load (Reg newName type') (Ref name type') type') 
                    v         -> (val2Reg v, Nothing))
            regName <- uniqueText
            allocInst <- allocRef r1
            let opInst = op (Reg regName $ typeOf r1) reg2 reg3
            let storeInst = Store (val2Ref r1) (Reg regName $ typeOf r1) (typeOf r1)
            return $ allocInst ++ catMaybes loadsM ++ [opInst, storeInst]

val2Reg :: K.Val -> Reg
val2Reg = \case
    K.Var{..} -> Reg name type'
    K.Num{..} -> I32 num
    K.Str{..} -> Str text
    K.NullVar -> error "NullVar"
    
val2Ref :: K.Val -> Ref
val2Ref = \case
    K.Var{..} -> Ref name type'
    v         -> error $ "NullVar: " ++ show v


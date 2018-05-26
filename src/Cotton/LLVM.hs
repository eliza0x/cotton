{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings, FlexibleContexts, DeriveFunctor #-}

module Cotton.LLVM where

import Data.Maybe
import Data.Text (Text, unpack)
import Data.Map.Strict ((!), (!?))
import qualified Data.Set as Se
import qualified Data.Map.Strict as M

import qualified Text.StringRandom as R

import qualified Control.Monad.State.Strict as S
import qualified Control.Monad.IO.Class as S
import qualified Control.Monad.Free as F

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
        " {\n"++ concatMap (\i -> indent i ++ show i ++"\n") is ++ 
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

data InstGeneratorBase cont
    = Emit Instraction cont
    | GenUniqueText (Text -> cont)
    | Allocate Text T.Type cont
    deriving Functor

type InstGenerator = F.Free InstGeneratorBase

emit :: Instraction -> InstGenerator ()
emit inst = F.wrap $ Emit inst (return ())

genUniqueText :: InstGenerator Text
genUniqueText = F.wrap $ GenUniqueText return

allocate :: Text -> T.Type -> InstGenerator ()
allocate name type' = F.wrap $ Allocate name type' (return ())

runInstGenerator :: InstGenerator a -> IO [Instraction]
runInstGenerator m = (\insts -> filter isAlloca insts ++ filter (not . isAlloca) insts) 
                <$> runInstGeneratorIter Se.empty m where
    runInstGeneratorIter :: Se.Set Text -> InstGenerator a -> IO [Instraction]
    runInstGeneratorIter alreadyAllocated = \case
        (F.Free (Emit inst cont)) -> (inst :) <$> runInstGeneratorIter alreadyAllocated cont
        (F.Free (GenUniqueText cont)) -> R.stringRandomIO "[a-zA-Z][a-zA-Z0-9_]{7}" 
                                     >>= runInstGeneratorIter alreadyAllocated . cont
        (F.Free (Allocate name type' cont)) -> let 
            alreadyAllocated' = name `Se.insert` alreadyAllocated
            allocateInst = if name `Se.member` alreadyAllocated
                then []
                else [Alloca (Ref name type') type']
            in (allocateInst++) <$> runInstGeneratorIter alreadyAllocated' cont
        (F.Pure _) -> return []
    isAlloca Alloca{..} = True
    isAlloca _          = False

knorm2llvmir :: [K.Block] -> IO [LLVM_IR]
knorm2llvmir blocks = do
    (ir, insts) <- unzip <$> mapM block2LLVM_IR blocks
    return $ Fun "init" (T.Type "Unit") [] (concat insts) : ir

block2LLVM_IR :: K.Block -> IO (LLVM_IR, [Instraction])
block2LLVM_IR = \case
    K.Fun{..}  -> do
        llvmir <- runInstGenerator $ mapM (kNormal2Instraction args) knorms
        return (Fun label btype (map val2Reg args) llvmir, [])
    K.Bind{..} -> do 
        llvmir <- runInstGenerator $ mapM (kNormal2Instraction []) knorms
        return (Bind label btype (initVal btype), llvmir)
    where
    initVal = \case
        T.Type "I32"    -> I32 0
        T.Type "Bool"   -> VBool False
        T.Type "String" -> Str ""

kNormal2Instraction :: [K.Val] ->  K.KNormal -> InstGenerator ()
kNormal2Instraction blockArgs = \case
        (K.Op "+"  r1 r2 r3 _) -> genOpInst Add r1 r2 r3
        (K.Op "-"  r1 r2 r3 _) -> genOpInst Sub r1 r2 r3
        (K.Op "*"  r1 r2 r3 _) -> genOpInst Mul r1 r2 r3
        (K.Op "/"  r1 r2 r3 _) -> genOpInst Div r1 r2 r3
        (K.Op "==" r1 r2 r3 _) -> genOpInst Eqi r1 r2 r3
        (K.Op fun  r1 r2 r3 _) -> genCallInst fun r1 [r2,r3]
        (K.Call r1 fun args _) -> genCallInst fun r1 args
        K.Let{..}              ->
            case (K.name val1, isArg val2, isVar val2) of
                -- 返り値は値のため, 引数は値であるためスルー
                ("_return", False, True) -> 
                    emit $ load val1 val2
                ("_return", False, False) -> do
                    refName <- genUniqueText
                    allocate refName (typeOf val2)
                    emit $ store (K.Var refName (K.type' val2) Nothing) val2
                    emit $ load val1 (K.Var refName (K.type' val2) Nothing)
                (_, False, True) -> do
                    regName <- genUniqueText
                    allocate (K.name val1) (typeOf val1)
                    emit $ load (K.Var regName (typeOf val2) Nothing) val2
                    emit $ store val1 (K.Var regName (typeOf val2) Nothing)
                (_, _, _) -> do
                    allocate (K.name val1) (typeOf val1)
                    emit $ store val1 val2
        (K.If condReg retReg cond then' else' _) -> do
            [t,e,c, crName] <- replicateM 4 genUniqueText
            mapM_ (kNormal2Instraction blockArgs) cond
            emit $ load (K.Var crName (typeOf condReg) Nothing) condReg
            emit $ CBr (Reg crName $ typeOf condReg) ("then_"<>t) ("else_"<>e)
            emit $ Label ("then_"<>t)
            mapM_ (kNormal2Instraction blockArgs) then'
            emit $ Br ("continue_"<>c)
            emit $ Label ("else_"<>e)
            mapM_ (kNormal2Instraction blockArgs) else'
            emit $ Br ("continue_"<>c)
            emit $ Label ("continue_"<>c)
        where
        isArg (K.Var name _ _) = name `elem` map K.name blockArgs
        isArg _                = False

        isVar K.Var{} = True
        isVar _       = False
        store rd rs = Store (val2Ref rd) (val2Reg rs) (typeOf rd)
        load  rd rs = Load  (val2Reg rd) (val2Ref rs) (typeOf rd)

        typeOf = \case
            K.Var{..} -> type'
            K.NullVar -> T.Bottom
            K.Num{..} -> T.Type "I32"
            K.Str{..} -> T.Type "String"

        genCallInst funName (K.Var "_return" type' _) args = do
            let rd = K.Var "_return" type' Nothing
            names <- replicateM (length args) genUniqueText
            args' <- loadVars names args
            emit $ Call funName (typeOf rd) (val2Reg rd) args'

        genCallInst funName rd args = do
            names <- replicateM (length args) genUniqueText
            args' <- loadVars names args
            regName <- genUniqueText
            emit $ Call funName (typeOf rd) (Reg regName (typeOf rd)) args'
            allocate (K.name rd) (typeOf rd)
            emit $ store rd (K.Var regName (typeOf rd) Nothing) 

        genOpInst op r1 r2 r3 = do
            regName:names <- replicateM 3 genUniqueText
            [reg2, reg3] <- loadVars names [r2,r3]
            emit $ op (Reg regName $ typeOf r1) reg2 reg3
            allocate (K.name r1) (typeOf r1)
            emit $ store r1 (K.Var regName (typeOf r1) Nothing)

        -- | 関数呼び出しの際に引数に変数が含まれていればそれをload
        --   loadした変数の名前を返す
        loadVars names args = forM (zip names args) (\(newName, arg) -> case (isArg arg, arg) of
            (False, K.Var{}) -> do
                emit $ load (K.Var newName (typeOf arg) Nothing) (K.Var (K.name arg) (typeOf arg) Nothing)
                return $ Reg newName (typeOf arg)
            (_, _) -> return $ val2Reg arg)


val2Reg :: K.Val -> Reg
val2Reg = \case
    K.Var{..} -> Reg name type'
    K.Num{..} -> I32 num
    K.Str{..} -> Str text
    K.NullVar -> error "NullVar"
    
val2Ref :: K.Val -> Ref
val2Ref = \case
    K.Var{..} -> Ref name type'
    v         -> error $ "con't convert ref type: " ++ show v

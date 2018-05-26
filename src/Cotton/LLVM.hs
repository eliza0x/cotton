{-|
Module      : Cotton.LLVM
Description : generate LLVM IR
Copyright   : (c) Sohei Yamaga, 2018
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental
Portability : POSIX

K正規化された式からLLVM IRを生成します。

-}

{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings, FlexibleContexts, DeriveFunctor #-}

module Cotton.LLVM where

import Data.Maybe
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Map.Strict ((!), (!?))
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import qualified Text.StringRandom as R

import qualified Control.Monad.Free as F

import qualified Control.Monad as C
import Data.Monoid
import Data.Functor

import qualified Cotton.KNormalize as K
import qualified Cotton.Type as T

data LLVM_IR 
    = Fun  { label :: Text, retType :: T.Type, args :: [Reg], insts :: [Instruction] }
    | Bind { label :: Text, retType :: T.Type, ival :: Reg }
    deriving (Show, Eq)

data Instruction 
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
    | Ret    { label' :: Text, type' :: T.Type }
    | Label  { label' :: Text }
    deriving (Show, Eq)

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
    = Emit Instruction cont
    | GenUniqueText (Text -> cont)
    | Allocate Text T.Type cont
    deriving Functor

type InstGenerator = F.Free InstGeneratorBase

emit :: Instruction -> InstGenerator ()
emit inst = F.wrap $ Emit inst (return ())

genUniqueText :: InstGenerator Text
genUniqueText = F.wrap $ GenUniqueText return

allocate :: Text -> T.Type -> InstGenerator ()
allocate name type' = F.wrap $ Allocate name type' (return ())

runInstGenerator :: InstGenerator a -> IO [Instruction]
runInstGenerator m = (\insts -> filter isAlloca insts ++ filter (not . isAlloca) insts) 
                <$> runInstGeneratorIter S.empty m where
    runInstGeneratorIter :: S.Set Text -> InstGenerator a -> IO [Instruction]
    runInstGeneratorIter alreadyAllocated = \case
        (F.Free (Emit inst cont)) -> (inst :) <$> runInstGeneratorIter alreadyAllocated cont
        (F.Free (GenUniqueText cont)) -> uniqueText >>= runInstGeneratorIter alreadyAllocated . cont
        (F.Free (Allocate name type' cont)) -> let 
            alreadyAllocated' = name `S.insert` alreadyAllocated
            allocateInst = if name `S.member` alreadyAllocated
                then []
                else [Alloca (Ref name type') type']
            in (allocateInst++) <$> runInstGeneratorIter alreadyAllocated' cont
        (F.Pure _) -> return []
    isAlloca Alloca{..} = True
    isAlloca _          = False

uniqueText :: IO Text
uniqueText = R.stringRandomIO "[a-zA-Z][a-zA-Z0-9_]{7}" 

-- | "_return"をLetの左辺にのみ現れるよう展開、これによりLLVM IRの生成を簡単化出来る
expandReturn :: [K.KNormal] -> IO [K.KNormal]
expandReturn knorms = concat <$> mapM er knorms where
    er :: K.KNormal -> IO [K.KNormal]
    er knorm = do
        n <- uniqueText
        case knorm of
            (K.Op op (K.Var "_return" t _) r2 r3 p)       -> return [K.Op op (K.Var n t p) r2 r3 p   , genLet n t]
            (K.Call  (K.Var "_return" t _) fun args p)    -> return [K.Call  (K.Var n t p) fun args p, genLet n t]
            (K.Let   (K.Var "_return" t _) r2 p)          -> return [K.Let   (K.Var n t p) r2 p      , genLet n t]
            (K.If    (K.Var "_return" t _) cr cs ts es p) -> do
                cs' <- expandReturn cs
                ts' <- expandReturn ts
                es' <- expandReturn es
                return [K.If (K.Var n t p) cr cs' ts' es' p, genLet n t]
            t -> return [t]
    genLet n type' = K.Let (K.Var "_return" type' Nothing) (K.Var n type' Nothing) Nothing

-- | LLVMでは引数が参照ではなく値であるためこれを変換するIRと、新しい引数の変数名を生成する
expandArg :: [K.Val] -> IO ([Reg], [Instruction])
expandArg args = do
    newNames <- C.replicateM (length args) uniqueText
    let (args', header) = unzip $ zipWith
            (\arg name -> let 
            type' = K.type' arg
            r = Reg name type'
            store = Store (val2Ref arg) r type'
            alloc = Alloca (val2Ref arg) type'
            in (r, [alloc, store])) args newNames
    return (args', concat header)

-- | LLVM IRを生成
knorm2llvmir :: [K.Block] -> IO [LLVM_IR]
knorm2llvmir blocks = do
    (ir, insts) <- unzip <$> mapM block2LLVM_IR blocks
    -- 初期化する値が無い場合は@initは生成しない
    t <- uniqueText
    let initFun = [Fun ("init_"<>t) (T.Type "Unit") [] (concat insts) | (not . null . concat) insts]
    return $ initFun ++ ir

block2LLVM_IR :: K.Block -> IO (LLVM_IR, [Instruction])
block2LLVM_IR = \case
    K.Fun{..}  -> do
        knorms' <- expandReturn knorms
        (args', header) <- expandArg args
        llvmir <- runInstGenerator $ mapM kNormal2Instruction knorms'
        return (Fun label btype args' (header++llvmir), [])
    K.Bind{..} -> do 
        knorms' <- expandReturn knorms
        llvmir <- runInstGenerator $ mapM kNormal2Instruction knorms'
        return (Bind label btype (initVal btype), llvmir)
    where
    initVal = \case
        T.Type "I32"    -> I32 0
        T.Type "Bool"   -> VBool False
        T.Type "String" -> Str ""

kNormal2Instruction ::  K.KNormal -> InstGenerator ()
kNormal2Instruction = \case
        (K.Op "+"  r1 r2 r3 _) -> genOpInst Add r1 r2 r3
        (K.Op "-"  r1 r2 r3 _) -> genOpInst Sub r1 r2 r3
        (K.Op "*"  r1 r2 r3 _) -> genOpInst Mul r1 r2 r3
        (K.Op "/"  r1 r2 r3 _) -> genOpInst Div r1 r2 r3
        (K.Op "==" r1 r2 r3 _) -> genOpInst Eqi r1 r2 r3
        (K.Op fun  r1 r2 r3 _) -> genCallInst fun r1 [r2,r3]
        (K.Call r1 fun args _) -> genCallInst fun r1 args
        K.Let{..}              ->
            case (K.name val1, isVar val2) of
                -- 前処理によって"_return"はletの左辺にのみ現れる
                ("_return", _) -> do
                    regName <- genUniqueText
                    emit $ load (K.Var regName (typeOf val2) Nothing) val2
                    emit $ Ret regName (typeOf val2)
                (_, True) -> do
                    regName <- genUniqueText
                    allocate (K.name val1) (typeOf val1)
                    emit $ load (K.Var regName (typeOf val2) Nothing) val2
                    emit $ store val1 (K.Var regName (typeOf val2) Nothing)
                (_, _) -> do
                    allocate (K.name val1) (typeOf val1)
                    emit $ store val1 val2
        (K.If condReg _ cond then' else' _) -> do
            [t,e,c, crName] <- C.replicateM 4 genUniqueText
            mapM_ kNormal2Instruction cond
            emit $ load (K.Var crName (typeOf condReg) Nothing) condReg
            emit $ CBr (Reg crName $ typeOf condReg) ("then_"<>t) ("else_"<>e)
            emit $ Label ("then_"<>t)
            mapM_ kNormal2Instruction then'
            emit $ Br ("continue_"<>c)
            emit $ Label ("else_"<>e)
            mapM_ kNormal2Instruction else'
            emit $ Br ("continue_"<>c)
            emit $ Label ("continue_"<>c)
        where
        isVar K.Var{} = True
        isVar _       = False
        store rd rs = Store (val2Ref rd) (val2Reg rs) (typeOf rd)
        load  rd rs = Load  (val2Reg rd) (val2Ref rs) (typeOf rd)

        typeOf = \case
            K.Var{..} -> type'
            K.NullVar -> T.Bottom
            K.Num{..} -> T.Type "I32"
            K.Str{..} -> T.Type "String"

        genCallInst funName rd args = do
            regName:names <- C.replicateM (1+length args) genUniqueText
            args' <- loadVars names args
            regName <- genUniqueText
            emit $ Call funName (typeOf rd) (Reg regName (typeOf rd)) args'
            allocate (K.name rd) (typeOf rd)
            emit $ store rd (K.Var regName (typeOf rd) Nothing) 

        genOpInst op r1 r2 r3 = do
            regName:names <- C.replicateM 3 genUniqueText
            [reg2, reg3] <- loadVars names [r2,r3]
            emit $ op (Reg regName $ typeOf r1) reg2 reg3
            allocate (K.name r1) (typeOf r1)
            emit $ store r1 (K.Var regName (typeOf r1) Nothing)

        -- | 関数呼び出しの際に引数に変数が含まれていればそれをload
        --   loadした変数の名前を返す
        loadVars names args = C.forM (zip names args) (\(newName, arg) -> case arg of
            K.Var{} -> do
                emit $ load (K.Var newName (typeOf arg) Nothing) (K.Var (K.name arg) (typeOf arg) Nothing)
                return $ Reg newName (typeOf arg)
            _ -> return $ val2Reg arg)

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

toText :: [LLVM_IR] -> Text
toText = T.concat  . map block2Text
    where
    showT :: Show a => a -> Text
    showT = T.pack . show

    block2Text :: LLVM_IR -> Text
    block2Text (Bind l t v)    = "@"<>l<>" = global "<>showT t<>" "<>reg2Text v<>" align 4\n"
    block2Text (Fun l t as is) = 
        "define "<>showT t<>" @"<>l<>
        "("<>T.drop 2 (T.concat (map (\(Reg n t) -> ", "<>showT t<>" %"<>n) as))<>") "<> 
        " {\n"<> T.concat (map (\i -> indent i <> inst2Text i <>"\n") is) <> 
        "}\n"
        where    
        indent Label{..}  = ""
        indent _          = "\t"

    inst2Text :: Instruction -> Text
    inst2Text = \case
        (Alloca rd    type') -> ref2Text rd <> " = alloca "<>showT type'<>", align 4"
        (Store  rd rs type') -> "store "<>showT type'<>" "<>reg2Text rs<>", "<>showT type'<>"* "<>ref2Text rd<>", align 4"
        (Load   rd rs type') -> reg2Text rd <> " = load "<>showT type'<>", "<>showT type'<>"* "<>ref2Text rs<>", align 4"
        (Add    rd rs rt)    -> reg2Text rd <> " = add nsw i32 "<>reg2Text rs<>", "<>reg2Text rt
        (Sub    rd rs rt)    -> reg2Text rd <> " = sub nsw i32 "<>reg2Text rs<>", "<>reg2Text rt
        (Mul    rd rs rt)    -> reg2Text rd <> " = mul nsw i32 "<>reg2Text rs<>", "<>reg2Text rt
        (Div    rd rs rt)    -> reg2Text rd <> " = div nsw i32 "<>reg2Text rs<>", "<>reg2Text rt
        (Eqi    rd rs rt)    -> reg2Text rd <> " = icmp eq i32 "<>reg2Text rs<>", "<>reg2Text rt
        (CBr cond t e)       -> "br i1 "<>reg2Text cond<>", label %"<>t<>", label %"<>e
        (Br     label')      -> "br label %"<>label'
        (Ret    label' t)    -> "ret "<>showT t<>(if t == T.Type "Unit" then "" else " %"<>label')
        (Label  label')      -> "\n"<>label'<>":"
        (Call lbl type' rd args') -> reg2Text rd<>" = call "<>showT type'<>" @"<>lbl<>
                                    "("<>T.drop 2 (T.concat $ map (\(Reg n t) -> ", "<>showT t<>" %"<>n) args')<>") "

    ref2Text :: Ref -> Text
    ref2Text (Ref t _) = "%" <> t

    reg2Text :: Reg -> Text
    reg2Text (I32 n)    = showT n
    reg2Text (Str t)    = showT t
    reg2Text (Reg t _)  = "%"<>t
    reg2Text (GReg t _) = "@"<>t -- グローバル変数
    reg2Text Null       = "null"   -- 書き込み専用



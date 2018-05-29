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

{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings, OverloadedLabels, FlexibleContexts, DeriveFunctor #-}

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
import qualified Cotton.Type.Type as T
import qualified Cotton.Type as T
import Cotton.Util
import Data.Extensible hiding (Instruction)
import Control.Lens hiding ((:>))

import Debug.Trace

data LLVM_IR 
    = Fun  { label :: Text, retType :: T.Type, args :: [Reg], insts :: [Instruction] }
    | Bind { label :: Text, retType :: T.Type, ival :: Reg }
    deriving (Show, Eq)

data Instruction 
    = Alloca { rd :: Reg            }
    | Load   { rd :: Reg, rs :: Reg }
    | Store  { rd :: Reg, rs :: Reg }
    | Add    { rd :: Reg, rs :: Reg, rt :: Reg }
    | Sub    { rd :: Reg, rs :: Reg, rt :: Reg }
    | Mul    { rd :: Reg, rs :: Reg, rt :: Reg }
    | Div    { rd :: Reg, rs :: Reg, rt :: Reg }
    | Eqi    { rd :: Reg, rs :: Reg, rt :: Reg }
    | CBr    { cond :: Reg, thenLabel :: Text, elseLabel :: Text }
    | Br     { label' :: Text }
    | Label  { label' :: Text }
    | Ret    { label' :: Text, type' :: T.Type }
    | Call   { label' :: Text, type' :: T.Type, rd :: Reg, args' :: [Reg] }
    | Call'  { label' :: Text, type' :: T.Type,            args' :: [Reg] }
    deriving (Show, Eq)

data Reg
    = I32     Int        
    | Boolean Bool       
    | Str     Text       
    | Reg     Text T.Type
    | GReg    Text T.Type
    deriving Eq

instance Show Reg where
    show (I32 n)    = show n
    show (Boolean n) = show n
    show (Str t)    = show t
    show (Reg t _)  = "%"++unpack t
    show (GReg t _) = "@"++unpack t -- グローバル変数

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

runInstGenerator :: [Text] -> InstGenerator a -> IO [Instruction]
runInstGenerator args m = appendRetVoid . (\insts -> filter isAlloca insts ++ filter (not . isAlloca) insts)
                    <$> runInstGeneratorIter initState m 
    where
    appendRetVoid insts = if isRet $ last insts then insts else insts ++ [Ret "" $ T.Type "Unit"]
    isRet (Ret _ _) = True
    isRet _         = False
    initState = foldr S.insert S.empty args 
    runInstGeneratorIter :: S.Set Text -> InstGenerator a -> IO [Instruction]
    runInstGeneratorIter alreadyAllocated = \case
        (F.Free (Emit inst cont)) -> (inst :) <$> runInstGeneratorIter alreadyAllocated cont
        (F.Free (GenUniqueText cont)) -> uniqueText >>= runInstGeneratorIter alreadyAllocated . cont
        (F.Free (Allocate name type' cont)) -> let 
            alreadyAllocated' = name `S.insert` alreadyAllocated
            allocateInst = case (name `S.member` alreadyAllocated, type') of
                (True , _) -> []
                (False, _           ) -> [Alloca (Reg name type')]
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
            {-
            (K.Op op     (Var "_return" t _) r2 r3 p)       -> return [K.Op op (K.Var n t p) r2 r3 p   , genLet n t]
            (K.Call      (Var "_return" t _) fun args p)    -> return [K.Call  (K.Var n t p) fun args p, genLet n t]
            (K.Let       (Var "_return" t _) r2 p)          -> return [K.Let   (K.Var n t p) r2 p      , genLet n t]
            (K.Ref       (Var "_return" t _) r2 p)          -> return [K.Ref   (K.Var n t p) r2 p      , genLet n t]
            (K.UnRef     (Var "_return" t _) r2 p)          -> return [K.UnRef (K.Var n t p) r2 p      , genLet n t]
            (K.Overwrite (Var "_return" t _) rs p)          -> return [K.Overwrite (K.Var n t p) rs p  , genLet n t]
            (K.If        (Var "_return" t _) cr cs ts es p) -> do
            
                cs' <- expandReturn cs
                ts' <- expandReturn ts
                es' <- expandReturn es
                return [K.If (K.Var n t p) cr cs' ts' es' p, genLet n t]
            -}
            t -> return [t]
    genLet n t = K.Let (#var # (#name @= "_return" <: #type' @= t <: #pos @= Nothing <: nil))
                       (#var # (#name @= n         <: #type' @= t <: #pos @= Nothing <: nil)) Nothing

-- | LLVMでは引数が参照ではなく値であるためこれを変換するIRと、新しい引数の変数名を生成する
expandArg :: [Val] -> IO ([Reg], [Instruction])
expandArg args = do
    newNames <- C.replicateM (length args) uniqueText
    (args', header) <- unzip <$> C.forM args (\arg -> do
        [name, name', name''] <- C.replicateM 3 uniqueText
        let type' = valType arg
        let r     = Reg name type'
            alloc = Alloca (val2Reg arg)
            store = Store  (Reg (valName arg) (T.Ref $ valType arg)) r
        return (r, [alloc, store]))
    return (args', concat header)

-- | LLVM IRを生成
knorm2llvmir :: [K.Block] -> IO [LLVM_IR]
knorm2llvmir blocks = do
    (ir, insts) <- unzip <$> mapM block2LLVM_IR blocks
    -- 初期化する値が無い場合は@initは生成しない
    t <- uniqueText
    let initFun = [Fun ("init_"<>t) (T.Type "Unit") [] (concat insts) | (not . Prelude.null . concat) insts]
    return $ initFun ++ ir

block2LLVM_IR :: K.Block -> IO (LLVM_IR, [Instruction])
block2LLVM_IR = \case
    K.Fun{..}  -> do
        knorms' <- expandReturn knorms
        (args', header) <- expandArg args
        llvmir <- runInstGenerator (map valName args) $ mapM kNormal2Instruction knorms'
        return (Fun label btype args' (header++llvmir), [])
    K.Bind{..} -> do 
        knorms' <- expandReturn knorms
        llvmir <- runInstGenerator [] $ mapM kNormal2Instruction knorms'
        return (Bind label btype (initVal btype), llvmir)
    where
    initVal = \case
        T.Type "I32"    -> I32 0
        T.Type "Bool"   -> Boolean False
        T.Type "String" -> Str ""

kNormal2Instruction ::  K.KNormal -> InstGenerator ()
kNormal2Instruction = \case
        (K.Op "+"  r1 r2 r3 _) -> emit (Label "; add" ) >> genOpInst Add r1 r2 r3
        (K.Op "-"  r1 r2 r3 _) -> emit (Label "; sub" ) >> genOpInst Sub r1 r2 r3
        (K.Op "*"  r1 r2 r3 _) -> emit (Label "; mul" ) >> genOpInst Mul r1 r2 r3
        (K.Op "/"  r1 r2 r3 _) -> emit (Label "; div" ) >> genOpInst Div r1 r2 r3
        (K.Op "==" r1 r2 r3 _) -> emit (Label "; eqi" ) >> genOpInst Eqi r1 r2 r3
        (K.Op fun  r1 r2 r3 _) -> emit (Label "; fun" ) >> genCallInst fun r1 [r2,r3]
        (K.Call rd fun args _) -> emit (Label "; call") >> genCallInst fun rd args
        (K.UnRef   r1 r2 _)    -> do
            emit $ Label "; 1"
            regName <- genUniqueText
            emit $ Load  (Reg (valName r1) (T.Ref $ valType r1)) (Reg (valName r2) (T.Ref $ valType r2))

        (K.Ref   r1 r2 _)      -> do
            emit $ Label "; 2"
            regName <- genUniqueText
            allocate (valName r1)  (valType r1)
            emit $ Store (Reg (valName r1) (T.Ref $ valType r1)) (Reg (valName r2) (T.Ref $ valType r2))

        (K.Overwrite rd rs _)  -> do
            [rd', rs'] <- C.replicateM 2 genUniqueText
            emit $ Load  (Reg rd' $ valType rd) (Reg (valName rd) (T.Ref $ valType rd)) 
            emit $ Load  (Reg rs' $ valType rs) (Reg (valName rs) (T.Ref $ valType rs)) 
            emit $ Store (Reg rd' (valType rd)) (Reg rs' (valType rs))

        K.Let{..}              ->
            case (valName val1, isVar val2) of
                -- 前処理によって"_return"はletの左辺にのみ現れる
                ("_return", _) -> do
                    emit $ Label "; 3"
                    regName <- genUniqueText
                    emit $ Load (Reg regName $ valType val2) (Reg (valName val2) (T.Ref $ valType val2)) 
                    emit $ Ret regName (valType val2)

                -- a <- a
                (_, True) -> do
                    emit $ Label "; 4"
                    regName <- genUniqueText
                    allocate (valName val1) (valType val1)
                    emit $ Load (Reg regName $ valType val2) (Reg (valName val2) (T.Ref $ valType val2)) 
                    emit $ Store (Reg (valName val1) (T.Ref $ valType val2)) (Reg regName (valType val2))
                
                -- a <- a
                (_, _) -> do
                    emit $ Label "; 5"
                    allocate (valName val1) (valType val1)
                    emit $ Store (Reg (valName val1) (T.Ref $ valType val1)) (val2Reg val2)

        (K.If condReg _ cond then' else' _) -> do
            [t,e,c, crName] <- C.replicateM 4 genUniqueText
            mapM_ kNormal2Instruction cond
            emit $ Load (Reg crName $ valType condReg) (Reg crName (T.Ref $ valType condReg))
            emit $ CBr (Reg crName $ valType condReg) ("then_"<>t) ("else_"<>e)
            emit $ Label ("then_"<>t)
            mapM_ kNormal2Instruction then'
            emit $ Br ("continue_"<>c)
            emit $ Label ("else_"<>e)
            mapM_ kNormal2Instruction else'
            emit $ Br ("continue_"<>c)
            emit $ Label ("continue_"<>c)

genCallInst :: Text -> Val -> [Val] -> InstGenerator ()
genCallInst funName rd args = do
    regName:names <- C.replicateM (1+length args) genUniqueText
    args' <- loadVars names args
    regName <- genUniqueText
    if isNull rd
        then emit $ Call' funName (T.Type "Unit") args'
        else do
        emit $ Call funName (valType rd) (Reg regName (valType rd)) args'
        allocate (valName rd) (valType rd)
        emit $ Store (Reg (valName rd) (T.Ref $ valType rd)) (Reg regName (valType rd)) 

genOpInst :: (Reg -> Reg -> Reg -> Instruction) -> Val -> Val -> Val -> InstGenerator ()
genOpInst op r1 r2 r3 = do
    regName:names <- C.replicateM 3 genUniqueText
    [reg2, reg3] <- loadVars names [r2,r3]
    emit $ op (Reg regName $ valType r1) reg2 reg3
    allocate (valName r1) (valType r1)
    emit $ Store (Reg (valName r1) (T.Ref $ valType r1)) (Reg regName (valType r1))

-- | 関数呼び出しの際に引数に変数が含まれていればそれをload
--   loadした変数を返す
loadVars :: [Text] -> [Val] -> InstGenerator [Reg]
loadVars names args = C.forM (zip names args) (\(newName, arg) -> flip matchField arg
    $ #var  @= const (do
        emit $ Load (Reg newName $ valType arg) (Reg (valName arg) (T.Ref $ valType arg))
        return $ Reg newName (valType arg))
   <: #num  @= const (return $ val2Reg arg)
   <: #str  @= const (return $ val2Reg arg)
   <: #null @= const (return $ val2Reg arg)
   <: nil)

unRef (T.Ref t) = t

val2Reg :: Val -> Reg
val2Reg = matchField
    $ #var  @= (\r -> Reg (r ^. #name) (r ^. #type'))
   <: #num  @= (\r -> I32 $ r ^. #value)
   <: #str  @= (\r -> Str $ r ^. #text)
   <: #null @= const (error "null")
   <: nil
 
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
        (Alloca rd)          -> reg2Text rd <> " = alloca "<>showT (typeOf rd)<>", align 4"
        (Store  rd rs)       -> "store "<>showT (typeOf rs)<>" "<>reg2Text rs<>", "<>showT (typeOf rd)<>" "<>reg2Text rd<>", align 4"
        (Load   rd rs)       -> reg2Text rd <> " = load "<>showT (typeOf rd)<>", "<>showT (typeOf rs)<>" "<>reg2Text rs<>", align 4"
        (Add    rd rs rt)    -> reg2Text rd <> " = add nsw i32 "<>reg2Text rs<>", "<>reg2Text rt
        (Sub    rd rs rt)    -> reg2Text rd <> " = sub nsw i32 "<>reg2Text rs<>", "<>reg2Text rt
        (Mul    rd rs rt)    -> reg2Text rd <> " = mul nsw i32 "<>reg2Text rs<>", "<>reg2Text rt
        (Div    rd rs rt)    -> reg2Text rd <> " = div nsw i32 "<>reg2Text rs<>", "<>reg2Text rt
        (Eqi    rd rs rt)    -> reg2Text rd <> " = icmp eq i32 "<>reg2Text rs<>", "<>reg2Text rt
        (CBr cond t e)       -> "br i1 "<>reg2Text cond<>", label %"<>t<>", label %"<>e
        (Br     label')      -> "br label %"<>label'
        (Label  label')      -> "\n"<>label'<>":"
        (Call lbl type' rd args')  -> reg2Text rd<>" = call "<>showT type'<>" @"<>lbl<>
                                     "("<>T.drop 2 (T.concat $ map (\(Reg n t) -> ", "<>showT t<>" %"<>n) args')<>") "
        (Call' lbl type' args') -> "call "<>showT type'<>" @"<>lbl<>
                                   "("<>T.drop 2 (T.concat $ map (\(Reg n t) -> ", "<>showT t<>" %"<>n) args')<>") "
        (Ret    label' (T.Type "Unit")) -> "ret void"
        (Ret    label' t)    -> "ret "<>showT t<>(if t == T.Type "Unit" then "" else " %"<>label')

    reg2Text :: Reg -> Text
    reg2Text (I32 n)    = showT n
    reg2Text (Str t)    = showT t
    reg2Text (Reg t _)  = "%"<>t
    reg2Text (GReg t _) = "@"<>t -- グローバル変数

typeOf = \case
    (I32     _  ) -> T.Type "I32"
    (Boolean _  ) -> T.Type "Bool"
    (Str     _  ) -> T.Type "String"
    (Reg     _ t) -> t
    (GReg    _ t) -> t


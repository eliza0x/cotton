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

{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings, OverloadedLabels, FlexibleContexts, DeriveFunctor, ScopedTypeVariables #-}

module Cotton.LLVM where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S

import qualified Text.StringRandom as R

import qualified Control.Monad.Free as F

import qualified Control.Monad as C
import Data.Monoid ((<>))

import qualified Cotton.KNormalize as K
import qualified Cotton.Type.Type as T
import qualified Cotton.Type as T
import Cotton.Util
import Data.Extensible hiding (Instruction)
import Control.Lens hiding ((:>), op)

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
        (F.Free (Allocate n t cont)) -> let 
            alreadyAllocated' = n `S.insert` alreadyAllocated
            allocateInst = case (n `S.member` alreadyAllocated, t) of
                (True , _) -> []
                (False, _           ) -> [Alloca (genReg n t)]
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
            (K.Op op     r1 r2 r3  p) -> return $ genTerm (\r -> [K.Op op     (#var#(r & #name.~n)) r2 r3  p, genLet n $ r ^. #type]) r1
            (K.Call      r1 f args p) -> return $ genTerm (\r -> [K.Call      (#var#(r & #name.~n)) f args p, genLet n $ r ^. #type]) r1
            (K.Let       r1 r2     p) -> return $ genTerm (\r -> [K.Let       (#var#(r & #name.~n)) r2     p, genLet n $ r ^. #type]) r1
            (K.Ref       r1 r2     p) -> return $ genTerm (\r -> [K.Ref       (#var#(r & #name.~n)) r2     p, genLet n $ r ^. #type]) r1
            (K.UnRef     r1 r2     p) -> return $ genTerm (\r -> [K.UnRef     (#var#(r & #name.~n)) r2     p, genLet n $ r ^. #type]) r1
            (K.Overwrite r1 rs     p) -> return $ genTerm (\r -> [K.Overwrite (#var#(r & #name.~n)) rs     p, genLet n $ r ^. #type]) r1
            (K.If   r1 cr cs ts es p) -> do
                cs' <- expandReturn cs
                ts' <- expandReturn ts
                es' <- expandReturn es
                return $ genTerm (\r -> [K.If (#var#(r & #name.~n)) cr cs' ts' es' p, genLet n $ r ^. #type]) r1
        where
        genTerm :: (Var -> [K.KNormal]) -> Val -> [K.KNormal]
        genTerm f = matchField
                $ #var  @= (\(r :: Var) -> if r ^. #name == "_return" then f r else [knorm] :: [K.KNormal])
               <: #int  @= const [knorm]
               <: #str  @= const [knorm]
               <: #bool @= const [knorm]
               <: #null @= const [knorm]
               <: nil
        genLet n t = K.Let (#var # (#name @= "_return" <: #type @= t <: #pos @= Nothing <: nil))
                           (#var # (#name @= n         <: #type @= t <: #pos @= Nothing <: nil)) Nothing

-- | LLVMでは引数が参照ではなく値であるためこれを変換するIRと、新しい引数の変数名を生成する
expandArg :: [Reg] -> IO ([Reg], [Instruction])
expandArg args = do
    (args', header) <- unzip <$> C.forM args (\arg -> do
        newName <- uniqueText
        let r     = genReg newName $ typeOf arg
            alloc = Alloca arg
            store = Store  (genReg (nameOf arg) (T.Ref $ typeOf arg)) r
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
        (args', header) <- expandArg $ map val2Reg args
        llvmir <- runInstGenerator (map nameOf args) $ mapM kNormal2Instruction knorms'
        return (Fun label type' args' (header++llvmir), [])
    K.Bind{..} -> do 
        knorms' <- expandReturn knorms
        llvmir <- runInstGenerator [] $ mapM kNormal2Instruction knorms'
        return (Bind label type' (initReg type'), llvmir)
    where
    initReg :: T.Type -> Reg
    initReg = \case
        T.Type "I32"    -> #int  # (#value  @= 0  <: #pos @= Nothing <: nil)
        T.Type "String" -> #str  # (#text   @= "" <: #pos @= Nothing <: nil)
        T.Type "Bool"   -> #bool # False
        _               -> error "undefined"

kNormal2Instruction ::  K.KNormal -> InstGenerator ()
kNormal2Instruction = \case
        (K.Op "+"  rd rs rt _) -> {- emit (Label "; add" ) >> -} genOpInst   Add (val2Reg rd) (val2Reg rs) (val2Reg rt)
        (K.Op "-"  rd rs rt _) -> {- emit (Label "; sub" ) >> -} genOpInst   Sub (val2Reg rd) (val2Reg rs) (val2Reg rt)
        (K.Op "*"  rd rs rt _) -> {- emit (Label "; mul" ) >> -} genOpInst   Mul (val2Reg rd) (val2Reg rs) (val2Reg rt)
        (K.Op "/"  rd rs rt _) -> {- emit (Label "; div" ) >> -} genOpInst   Div (val2Reg rd) (val2Reg rs) (val2Reg rt)
        (K.Op "==" rd rs rt _) -> {- emit (Label "; eqi" ) >> -} genOpInst   Eqi (val2Reg rd) (val2Reg rs) (val2Reg rt)
        (K.Op fun  rd rs rt _) -> {- emit (Label "; fun" ) >> -} genCallInst fun (val2Reg rd) [val2Reg rs, val2Reg rt]
        (K.Call rd fun args _) -> {- emit (Label "; call") >> -} genCallInst fun (val2Reg rd) (map val2Reg args)
        (K.UnRef   r1 r2 _)    ->
            -- emit $ Label "; 1"
            emit $ Load  (genReg (nameOf r1) (T.Ref $ typeOf r1)) (genReg (nameOf r2) (T.Ref $ typeOf r2))

        (K.Ref   r1 r2 _)      -> do
            -- emit $ Label "; 2"
            allocate (nameOf r1)  (typeOf r1)
            emit $ Store (genReg (nameOf r1) (T.Ref $ typeOf r1)) (genReg (nameOf r2) (T.Ref $ typeOf r2))

        (K.Overwrite rd rs _)  -> do
            -- emit $ Label "; over"
            [rd', rs'] <- C.replicateM 2 genUniqueText
            emit $ Load  (genReg rd' $ typeOf rd) (genReg (nameOf rd) (T.Ref $ typeOf rd)) 
            emit $ Load  (genReg rs' $ typeOf rs) (genReg (nameOf rs) (T.Ref $ typeOf rs)) 
            emit $ Store (genReg rd' (typeOf rd)) (genReg rs' (typeOf rs))

        (K.Let val1 val2 _)    ->
            case (nameOf val1, isVar val2) of
                -- 前処理によって"_return"はletの左辺にのみ現れる
                ("_return", _) -> do
                    -- emit $ Label "; 3"
                    regName <- genUniqueText
                    emit $ Load (genReg regName $ typeOf val2) (genReg (nameOf val2) (T.Ref $ typeOf val2)) 
                    emit $ Ret regName (typeOf val2)

                -- 変数の変数への代入
                (_, True) -> do
                    -- emit $ Label "; 4"
                    regName <- genUniqueText
                    allocate (nameOf val1) (typeOf val1)
                    emit $ Load  (genReg regName $ typeOf val2) (genReg (nameOf val2) (T.Ref $ typeOf val2)) 
                    emit $ Store (genReg (nameOf val1) (T.Ref $ typeOf val2)) (genReg regName (typeOf val2))
                
                -- 即値の変数への代入
                (_, _) -> do
                    -- emit $ Label "; 5"
                    allocate (nameOf val1) (typeOf val1)
                    emit $ Store (genReg (nameOf val1) (T.Ref $ typeOf val1)) (val2Reg val2)

        (K.If condReg _ cond then' else' _) -> do
            -- emit $ Label "; if"
            [t,e,c, crName] <- C.replicateM 4 genUniqueText
            mapM_ kNormal2Instruction cond
            emit $ Load (genReg crName $ typeOf condReg) (genReg (nameOf condReg) (T.Ref $ typeOf condReg))
            emit $ CBr (genReg crName $ typeOf condReg) ("then_"<>t) ("else_"<>e)
            emit $ Label ("then_"<>t)
            mapM_ kNormal2Instruction then'
            emit $ Br ("continue_"<>c)
            emit $ Label ("else_"<>e)
            mapM_ kNormal2Instruction else'
            emit $ Br ("continue_"<>c)
            emit $ Label ("continue_"<>c)

genCallInst :: Text -> Reg -> [Reg] -> InstGenerator ()
genCallInst funName rd args = do
    regName:names <- C.replicateM (1+length args) genUniqueText
    args' <- loadVars names args
    if isNull rd
        then emit $ Call' funName (T.Type "Unit") args'
        else do
        emit $ Call funName (typeOf rd) (genReg regName (typeOf rd)) args'
        allocate (nameOf rd) (typeOf rd)
        emit $ Store (genReg (nameOf rd) (T.Ref $ typeOf rd)) (genReg regName (typeOf rd)) 

genOpInst :: (Reg -> Reg -> Reg -> Instruction) -> Reg -> Reg -> Reg -> InstGenerator ()
genOpInst op r1 r2 r3 = do
    regName:names <- C.replicateM 3 genUniqueText
    [reg2, reg3] <- loadVars names [r2,r3]
    emit $ op (genReg regName $ typeOf r1) reg2 reg3
    allocate (nameOf r1) (typeOf r1)
    emit $ Store (genReg (nameOf r1) (T.Ref $ typeOf r1)) (genReg regName (typeOf r1))

-- | 関数呼び出しの際に引数に変数が含まれていればそれをload
--   loadした変数を返す
loadVars :: [Text] -> [Reg] -> InstGenerator [Reg]
loadVars names args = C.forM (zip names args) (\(newName, arg) -> flip matchField arg
    $ #globalReg @= error "undefined"
   <: #reg  @= const (do
        emit $ Load (genReg newName $ typeOf arg) (genReg (nameOf arg) (T.Ref $ typeOf arg))
        return $ genReg newName (typeOf arg))
   <: #int  @= const (return arg)
   <: #str  @= const (return arg)
   <: #bool @= const (return arg)
   <: #null @= const (return arg)
   <: nil)

toText :: [LLVM_IR] -> Text
toText = T.concat  . map block2Text
    where
    showT :: Show a => a -> Text
    showT = T.pack . show

    block2Text :: LLVM_IR -> Text
    block2Text (Bind l t v)    = "@"<>l<>" = global "<>showT t<>" "<>nameOf v<>" align 4\n"
    block2Text (Fun l t as is) = 
        "define "<>showT t<>" @"<>l<>
        "("<>T.drop 2 (T.concat (map (\r -> ", "<>showT (typeOf r)<>" %"<>nameOf r) as))<>") "<> 
        " {\n"<> T.concat (map (\i -> indent i <> inst2Text i <>"\n") is) <> 
        "}\n"
        where    
        indent Label{..}  = ""
        indent _          = "\t"

    inst2Text :: Instruction -> Text
    inst2Text = \case
        (Alloca rd)          -> showBase rd <> " = alloca "<>showT (typeOf rd)<>", align 4"
        (Store  rd rs)       -> "store "<>showT (typeOf rs)<>" "<>showBase rs<>", "<>showT (typeOf rd)<>" "<>showBase rd<>", align 4"
        (Load   rd rs)       -> showBase rd <> " = load "<>showT (typeOf rd)<>", "<>showT (typeOf rs)<>" "<>showBase rs<>", align 4"
        (Add    rd rs rt)    -> showBase rd <> " = add nsw i32 "<>showBase rs<>", "<>showBase rt
        (Sub    rd rs rt)    -> showBase rd <> " = sub nsw i32 "<>showBase rs<>", "<>showBase rt
        (Mul    rd rs rt)    -> showBase rd <> " = mul nsw i32 "<>showBase rs<>", "<>showBase rt
        (Div    rd rs rt)    -> showBase rd <> " = div nsw i32 "<>showBase rs<>", "<>showBase rt
        (Eqi    rd rs rt)    -> showBase rd <> " = icmp eq i32 "<>showBase rs<>", "<>showBase rt
        (CBr cond t e)       -> "br i1 "<>showBase cond<>", label %"<>t<>", label %"<>e
        (Br     label')      -> "br label %"<>label'
        (Label  label')      -> "\n"<>label'<>":"
        (Call lbl t rd args')  -> showBase rd<>" = call "<>showT t<>" @"<>lbl<>
                                     "("<>T.drop 2 (T.concat $ map (\r -> ", "<>showT (typeOf r)<>" "<>showBase r) args')<>") "
        (Call' lbl t args') -> "call "<>showT t<>" @"<>lbl<>
                                   "("<>T.drop 2 (T.concat $ map (\r -> ", "<>showT (typeOf r)<>" "<>showBase r) args')<>") "
        (Ret _ (T.Type "Unit")) -> "ret void"
        (Ret    label' t)    -> "ret "<>showT t<>(if t == T.Type "Unit" then "" else " %"<>label')

genReg :: Text -> T.Type -> Reg
genReg n t = #reg #
    ( #name @= n
   <: #type @= t
   <: #pos  @= Nothing
   <: nil)


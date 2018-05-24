{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings, FlexibleContexts #-}

module Cotton.LLVM where

import Data.Maybe
import Control.Monad
import Data.Text (Text, unpack)
import Data.Map.Strict ((!), (!?))
import qualified Data.Map.Strict as M
import qualified Text.StringRandom as R

import qualified Control.Monad.State.Strict as S
import qualified Control.Monad.IO.Class as S

import qualified Cotton.KNormalize as K
import qualified Cotton.Type as T

data LLVM_IR 
    = Fun  { label :: Text, retType :: T.Type, args :: [Reg], insts :: [Instraction] }
    | Bind { label :: Text, retType :: T.Type, ival :: Reg }
    deriving Eq

instance Show LLVM_IR where
    show (Fun l t as is) = 
        "define "++show t++" @"++unpack l++
        "("++drop 2 (concatMap (\(Reg n t) -> ", "++show t++" %"++unpack n) as)++") "++ 
        " {"++ addIndent (concatMap (("\n"++) . show) is) ++ 
        "\tret "++show t++" %_return"++
        "\n}\n"
    show (Bind l t v)    = "@"++unpack l++" = global "++show t++" "++show v++" align 4\n"

data Instraction 
    = Alloca { rd   :: Ref,            type' :: T.Type }
    | Store  { rd   :: Ref, rs  :: Reg, type' :: T.Type }
    | Load   { rd'  :: Reg, rs' :: Ref, type' :: T.Type }
    | Call   { label' :: Text,  type' :: T.Type, rd :: Ref, args' :: [Reg] }
    | Add    { rd   :: Ref, rs  :: Reg, rt :: Reg }
    | Sub    { rd   :: Ref, rs  :: Reg, rt :: Reg }
    | Mul    { rd   :: Ref, rs  :: Reg, rt :: Reg }
    | Div    { rd   :: Ref, rs  :: Reg, rt :: Reg }
    | Eqi    { rd   :: Ref, rs  :: Reg, rt :: Reg }
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
        (Sub    rd rs rt)    -> undefined
        (Mul    rd rs rt)    -> undefined
        (Div    rd rs rt)    -> undefined
        (Eqi    rd rs rt)    -> undefined
        (CBr cond t e)       -> undefined
        (Br     label')      -> undefined
        (Label  label')      -> undefined
        (Call lbl type' rd args') -> undefined

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
 

addIndent = unlines . map ("\t"++) . lines

knorm2llvmir :: [K.Block] -> IO [LLVM_IR]
knorm2llvmir blocks = do
    (ir, insts) <- unzip <$> mapM block2LLVM_IR blocks
    return $ Fun "init" (T.Type "Unit") [] (concat insts) : ir

block2LLVM_IR :: K.Block -> IO (LLVM_IR, [Instraction])
block2LLVM_IR = \case
    K.Fun{..}  -> do
        llvmir <- kNormal2Instraction args knorms
        return (Fun label btype (map val2Reg args) llvmir, [])
    K.Bind{..} -> do 
        llvmir <- kNormal2Instraction [] knorms
        return (Bind label btype (initVal btype), llvmir)
    where
    genDict args = foldr (\(i, arg) dict -> M.insert (K.name arg) i dict) M.empty $ zip [0..] args
    initVal = \case
        T.Type "Int"    -> I32 0
        T.Type "Bool"   -> VBool False
        T.Type "String" -> Str ""

type InstM = S.StateT (M.Map Text Int) IO

kNormal2Instraction :: [K.Val] ->  [K.KNormal] -> IO [Instraction]
kNormal2Instraction args knorms =
    (header ++) . concat <$> mapM (\knorm -> S.evalStateT (kNormal2Instraction' knorm) initState) knorms
    where
    initState :: M.Map Text Int
    initState = foldr (\(arg, id) dict -> M.insert (K.name arg) id dict) M.empty (zip args [1..])

    allocRef :: K.Val -> InstM [Instraction]
    allocRef K.Var{..} = do
        return [Alloca (Ref name type') type'] 
        {-
        isDefined <- isJust . (!? name) <$> S.get
        if isDefined then do
            m <- maximum <$> S.get
            -- S.modify $ M.insert name (m+0)
            return [Alloca (Ref name type') type'] 
            else return []
        -}
    allocRef _ = return []

    kNormal2Instraction' :: K.KNormal -> InstM [Instraction]
    kNormal2Instraction' knorm = case knorm of
        (K.Op "+"  r1 r2 r3 _) -> genOpInst Add r1 r2 r3
        (K.Op "-"  r1 r2 r3 _) -> genOpInst Sub r1 r2 r3
        (K.Op "*"  r1 r2 r3 _) -> genOpInst Mul r1 r2 r3
        (K.Op "/"  r1 r2 r3 _) -> genOpInst Div r1 r2 r3
        (K.Op "==" r1 r2 r3 _) -> genOpInst Eqi r1 r2 r3
        (K.Op fun  r1 r2 r3 _) -> do
            alloc1 <- allocRef r1
            [r2Name, r3Name] <- replicateM 2 uniqueText
            let (reg2, reg3) = (Reg r2Name (typeOf r2), Reg r3Name (typeOf r3))
            let load2  = Load reg2 (val2Ref r2) (typeOf r2)
            let load3  = Load reg3 (val2Ref r3) (typeOf r3) 
            let callInst = Call fun (typeOf r1) (val2Ref r1) [reg2,reg2]
            return $ alloc1 ++ [load2, load3, callInst]
        (K.Call r1 fun args _) -> do
            alloc1 <- allocRef r1
            names <- replicateM (length args) uniqueText
            let (args', loadsM) = unzip $ flip map (zip names args) (\(newName, arg) -> case arg of
                    K.Var{..} -> (Reg newName type', Just $ Load (Reg newName type') (Ref name type') type') 
                    v         -> (val2Reg v, Nothing))
            let callInst = Call fun (typeOf r1) (val2Ref r1) args'
            return $ alloc1 ++ catMaybes loadsM ++ [callInst]
        K.Let{..}              -> do
            allocInst <- allocRef val1
            case val2 of
                K.Var{..} -> do
                    regName <- uniqueText
                    let loadInst = Load (Reg regName (typeOf val2)) (val2Ref val2) (typeOf val2)
                    let storeInst = Store (val2Ref val1) (Reg regName (typeOf val2)) (typeOf val1)
                    return $ allocInst ++ [loadInst, storeInst]
                _ -> do
                    let storeInst = Store (val2Ref val1) (val2Reg val2) (typeOf val2)
                    return $ allocInst ++ [storeInst]
        (K.If condReg retReg cond then' else' _) -> do
            condInsts <- concat <$> mapM kNormal2Instraction' cond
            thenInsts <- concat <$> mapM kNormal2Instraction' then'
            elseInsts <- concat <$> mapM kNormal2Instraction' else'
            return $ condInsts ++ [CBr (val2Reg condReg) "then" "else"] ++ 
                     [Label "then"] ++ thenInsts ++ [Br "continue"] ++
                     [Label "else"] ++ elseInsts ++ [Br "continue"] ++
                     [Label "continue"]
        where
        typeOf = \case
            K.Var{..} -> type'
            K.NullVar -> T.Bottom
            K.Num{..} -> T.Type "Int"
            K.Str{..} -> T.Type "String"

        uniqueText :: InstM Text
        uniqueText = S.liftIO $ R.stringRandomIO "[a-zA-Z][a-zA-Z0-9_]{7}"

        genOpInst op r1 r2 r3 = do
            [r2Name, r3Name] <- replicateM 2 uniqueText
            alloc1 <- allocRef r1
            let (reg2, reg3) = (Reg r2Name (K.type' r2), Reg r3Name (K.type' r3))
            let load2  = Load reg2 (val2Ref r2) (K.type' r2)
            let load3  = Load reg3 (val2Ref r3) (K.type' r3) 
            let opInst = op (val2Ref r1) reg2 reg3
            return $ alloc1 ++ [load2, load3, opInst]
            
    header :: [Instraction]
    header = flip concatMap args $ \arg -> case arg of
        K.Var{..} -> [Alloca (Ref name type') type', Store (Ref name type') (val2Reg arg) type']
        v         -> error $ show v
    
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


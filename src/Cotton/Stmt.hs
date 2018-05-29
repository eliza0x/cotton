module Cotton.Stmt where

import qualified Cotton.Lexer as CL
import Cotton.Type.Type
import Data.Text (Text, unpack)

-- | 文
data Stmt =
      Bind { label :: Text, type' :: Type, stmt :: [Stmt], pos :: CL.AlexPosn  }
    | Fun  { label :: Text, args  :: [Arg], type' :: Type, stmt :: [Stmt], pos ::CL.AlexPosn }
    | ETerm Term
    deriving Eq

-- | 式
data Term
    = TInt      { num  :: Int,                                         tpos :: CL.AlexPosn } -- 整数
    | Var       { var  :: Text,                                        tpos :: CL.AlexPosn } -- 名前
    | TStr      { text :: Text,                                        tpos :: CL.AlexPosn } -- 名前
    | Overwrite { var  :: Text, term   :: Term,                        tpos :: CL.AlexPosn } -- 変数上書き
    | Op        { op   :: Text, term   :: Term,   term'   :: Term,     tpos :: CL.AlexPosn } -- 演算子
    | Call      { var  :: Text, targs  :: [Term],                      tpos :: CL.AlexPosn } -- Call
    | SemiColon { term :: Term, term'  :: Term, type''' :: Maybe Type, tpos :: CL.AlexPosn } -- 連結
    | If        { cond :: Term, tstmts :: [Stmt], tstmts' :: [Stmt],   tpos :: CL.AlexPosn } -- if式
    deriving Eq

data Arg = Arg { argName :: Text, type'' :: Maybe Type, apos :: CL.AlexPosn }
    deriving Eq


instance Show Term where
    show (TInt n _)           = show n
    show (Var l  _)           = unpack l
    show (TStr t _)           = unpack t
    show (Op op t t' _)       = show t ++ " " ++ unpack op ++ " " ++ show t'
    show (Call l  as _)       = unpack l ++ "(" ++ (drop 2 . concat $ map (\a -> ", " ++ show a) as) ++ ")" 
    show (TInt n _)           = show n
    show (SemiColon t t' _ _) = show t ++ ";\n" ++ show t'
    show (If c e e' _)        = "if " ++ show c ++ " {\n" ++ (addIndent . unlines $ map show e) 
                              ++ "} else {\n" ++ (addIndent . unlines $ map show e') ++ "}"
    show (Overwrite l t _)    = unpack l ++ " <- " ++ show t

instance Show Stmt where
    show (ETerm t)          = show t
    show (Bind l t es _p)   = concat ["def ",unpack l,": ",show t," {\n",addIndent . unlines $ map show es,"}"]
    show (Fun l as t es _p) = concat ["def ",unpack l,"(",drop 2 . concat $ map (\a -> ", " ++ show a) as
                                     ,"): ",show t," {\n",addIndent . unlines $ map show es,"}"]

instance Show Arg where
    show (Arg a (Just t) _) = unpack a ++ ": " ++ show t
    show (Arg a Nothing  _) = unpack a

addIndent = unlines . map ("\t"++) . lines

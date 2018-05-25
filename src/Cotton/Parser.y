{
module Cotton.Parser (
    parser,
    Stmt(..),
    Term(..),
    Arg(..),
    ) where

import Prelude hiding (Num)
import Data.Text (Text(..), unpack)
import qualified Cotton.Lexer as CL
}

-- 生成するパーサの名前を指定
%name lawParser

-- トークンの型を指定
%tokentype { CL.Token }

-- エラー時parseErrorを呼び出す
%error { parseError }

-- パーサーをEither Textで包む
%monad { Either Text } 

-----------------------------------------------------------------------------

%token
  def       { CL.Def        $$ }
  if        { CL.If         $$ }
  else      { CL.Else       $$ }
  '<-'      { CL.LArrow     $$ }
  '->'      { CL.RArrow     $$ }
  '='       { CL.Equal      $$ }
  '`'       { CL.BackQuote  $$ }
  '"'       { CL.Quort      $$ }
  '\''      { CL.Apostrophe $$ }
  ';'       { CL.Semicolon  $$ }
  ':'       { CL.Colon      $$ }
  ','       { CL.Comma      $$ }
  '('       { CL.LParen     $$ }
  ')'       { CL.RParen     $$ }
  '{'       { CL.LBrace     $$ }
  '}'       { CL.RBrace     $$ }
  '['       { CL.LBracket   $$ }
  ']'       { CL.RBracket   $$ }
  num       { CL.Num     $$ }
  op        { CL.Op      $$ }
  var       { CL.Lower   $$ }
  type      { CL.Upper   $$ }
  str       { CL.Str     $$ }

%%

-----------------------------------------------------------------------------

-- | 文
-- トップレベルに式を書かせない
Stmts :: { [Stmt] }
Stmts   : Bind Stmts             { fst $1 : $2 }
        | Fun  Stmts             { fst $1 : $2 }
        |                       { [] }
        | Terms                 {% fail $ "Top-level declaration expected:" ++ (show $ snd $1) }

-- | インナー文
Stmts2 :: { [Stmt] }
Stmts2  : Bind Stmts2            { fst $1 : $2 }
        | Fun  Stmts2            { fst $1 : $2 }
        | Terms Stmts2           { ETerm (fst $1) : $2 }
        | Terms                  { [ETerm (fst $1)] }
        | Bind                   {% fail $ "statement must be finish term." ++ show (snd $1) }
        | Fun                    {% fail $ "statement must be finish term." ++ show (snd $1) }

Bind :: { (Stmt, CL.AlexPosn) }
Bind    : def Lower ':' Upper '=' Term ';'   
        { (Bind { label = fst $2, type' = fst $4, stmt = [ETerm $ fst $6], pos = CL.pos $1 }, CL.pos $1) }
        | def Lower ':' Upper '{' Stmts2 '}' 
        { (Bind { label = fst $2, type' = fst $4, stmt = $6,               pos = CL.pos $1 }, CL.pos $1) }

Fun :: { (Stmt, CL.AlexPosn) }
Fun     : def Lower '(' Args ')' ':' Upper '=' Term ';'
        { (Fun { label = fst $2 , args = $4, type' = fst $7, stmt = [ETerm $ fst $9], pos = CL.pos $1 }, CL.pos $1) }
        | def Lower '(' Args ')' ':' Upper '{' Stmts2 '}'
        { (Fun { label = fst $2 , args = $4, type' = fst $7, stmt  = $9,        pos = CL.pos $1 }, CL.pos $1) }

-- | 式
Terms   :: { (Term, CL.AlexPosn) } 
Terms   : Term ';' Terms        { (SemiColon {term = fst $1, term' = fst $3}, snd $1) }
        | Term                  { (fst $1, snd $1) }

Term   :: { (Term, CL.AlexPosn) } 
Term    : if Term '{' Stmts2 '}' else '{' Stmts2 '}' 
                                    { (If {cond = fst $2, tstmts = $4,
                                    tstmts' = $8, tpos = CL.pos $1 }, CL.pos $1) }
        | Lower '<-' Term           { (Overwrite (fst $1) (fst $3) (snd $1), snd $1) }
        | Lower '(' Calls ')'       { (Call { var = fst $1, targs = $3, tpos = snd $1 }, snd $1) }
        | '(' Term ')'              { (fst $2, snd $2) }
        | '{' Terms '}'             { (fst $2, snd $2) }
        | str                       { (TStr (CL.text $1) (CL.pos $1), CL.pos $1) }
        | Term Op Term              { (Op { op = fst $2, term = fst $1, term' = fst $3, tpos = snd $2 }, snd $1) }
        | Num                       { (TInt (fst $1) (snd $1), snd $1) }
        | Lower                     { (Var (fst $1) (snd $1), snd $1) }

-----------------------------------------------------------------------------

-- | 関数呼び出し時の引数リスト
Calls :: { [Term] }
Calls   : Term ',' Calls        { fst $1 : $3 }
        | Term                  { [fst $1] }
        |                       { [] }
 
-- | 関数定義時の引数リスト
Args :: { [Arg] }
Args    : Arg ',' Args          { $1 : $3 }
        | Arg                   { [$1] }
        |                       { [] }
 
Arg :: { Arg }
Arg     : Lower ':' Upper          { Arg { argName  = fst $1, type'' = Just (fst $3), apos = snd $1} }
        | Lower                    { Arg { argName  = fst $1, type'' = Nothing,       apos = snd $1} }

-----------------------------------------------------------------------------

-- | 整数
Num :: { (Int, CL.AlexPosn) } 
Num     : num   { (CL.num $1, CL.pos $1) }

-- | 演算子
Op :: { (Text, CL.AlexPosn) } 
Op      : op    { (CL.text $1, CL.pos $1) } 
        | var   {% fail $ "Require operator: " ++ show (CL.pos $1) }
        | type  {% fail $ "Require operator: " ++ show (CL.pos $1) }

-- | 関数、変数
Lower :: { (Text, CL.AlexPosn) } 
Lower   : var   { (CL.text $1, CL.pos $1) } 
        | type  {% fail $ "Require upper case: " ++ show (CL.pos $1) }
        | op    {% fail $ "Require upper case: " ++ show (CL.pos $1) }

-- | 型
Upper :: { (Text, CL.AlexPosn) } 
Upper   : type  { (CL.text $1, CL.pos $1) } 
        | var   {% fail $ "Require lower case: " ++ show (CL.pos $1) }
        | op    {% fail $ "Require upper case: " ++ show (CL.pos $1) }

-----------------------------------------------------------------------------

{

-- | 文
data Stmt =
      Bind { label :: Text, type' :: Text, stmt :: [Stmt], pos :: CL.AlexPosn  }
    | Fun  { label :: Text, args  :: [Arg], type' :: Text, stmt :: [Stmt], pos ::CL.AlexPosn }
    | ETerm Term
    deriving Eq

-- | 式
data Term
    = TInt      { num  :: Int,  tpos   :: CL.AlexPosn } -- 整数
    | Var       { var  :: Text, tpos   :: CL.AlexPosn } -- 名前
    | TStr      { text :: Text, tpos   :: CL.AlexPosn } -- 名前
    | Overwrite { var  :: Text, term   :: Term,   tpos    :: CL.AlexPosn }                 -- 変数上書き
    | Op        { op   :: Text, term   :: Term,   term'   :: Term, tpos :: CL.AlexPosn }   -- 演算子
    | Call      { var  :: Text, targs  :: [Term], tpos    :: CL.AlexPosn }                 -- Call
    | SemiColon { term :: Term, term'  :: Term,   tpos    :: CL.AlexPosn }                 -- 連結
    | If        { cond :: Term, tstmts :: [Stmt], tstmts' :: [Stmt], tpos :: CL.AlexPosn } -- if式
    deriving Eq

data Arg = Arg { argName :: Text, type'' :: Maybe Text, apos :: CL.AlexPosn }
    deriving Eq

parseError :: [CL.Token] -> a
parseError ts = error $ "Parse error - line: " ++ show ts

parser :: [CL.Token] -> Either Text [Stmt]
parser tokens = lawParser tokens

instance Show Term where
    show (TInt n _)         = show n
    show (Var l  _)         = unpack l
    show (TStr t _)         = unpack t
    show (Op op t t' _)     = show t ++ " " ++ unpack op ++ " " ++ show t'
    show (Call l  as _)     = unpack l ++ "(" ++ (drop 2 . concat $ map (\a -> ", " ++ show a) as) ++ ")" 
    show (TInt n _)         = show n
    show (SemiColon t t' _) = show t ++ ";\n" ++ show t'
    show (If c e e' _)      = "if " ++ show c ++ " {\n" ++ (addIndent . unlines $ map show e) 
                            ++ "} else {\n" ++ (addIndent . unlines $ map show e') ++ "}"
    show (Overwrite l t _)  = unpack l ++ " <- " ++ show t

instance Show Stmt where
    show (ETerm t)          = show t
    show (Bind l t es _p)   = concat ["def ",unpack l,": ",unpack t," {\n",addIndent . unlines $ map show es,"}"]
    show (Fun l as t es _p) = concat ["def ",unpack l,"(",drop 2 . concat $ map (\a -> ", " ++ show a) as
                                     ,"): ",unpack t," {\n",addIndent . unlines $ map show es,"}"]

instance Show Arg where
    show (Arg a (Just t) _) = unpack a ++ ": " ++ unpack t
    show (Arg a Nothing  _) = unpack a

addIndent = unlines . map ("\t"++) . lines
}

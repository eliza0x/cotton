{
module Cotton.Parser where

import Prelude hiding (Num)
import Data.Text (Text(..), unpack)
import qualified Cotton.Lexer as CL
}

%name lawParser
%tokentype { CL.Token }
%error { parseError }
%monad { Either Text } -- { thenE } { return }

%token
  def       { CL.Def        $$ }
  if        { CL.If      $$ }
  else      { CL.Else       $$ }
  '<-'      { CL.LArrow     $$ }
  '->'      { CL.RArrow     $$ }
  '='       { CL.Equal      $$ }
  '`'       { CL.Backquort  $$ }
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

-- トップレベルに式を書かせない
Exprs :: { [Expr] }
Exprs   : Expr Exprs            { $1 : $2 }
        | Expr                  { [$1] }

Expr :: { Expr }
Expr    : Bind                  { $1 }
        | Fun                   { $1 }
        | Terms                 {% fail $ "Top-level declaration expected:" ++ (show $1) }

Exprs2 :: { [Expr] }
Exprs2   : Expr2 Exprs2         { $1 : $2 }
         | Expr2                { [$1] }

Expr2 :: { Expr }
Expr2   : Bind                  { $1 }
        | Fun                   { $1 }
        | Terms                 { ETerm $1 }


Bind :: { Expr }
Bind    : def Val '=' Term ';'   { Bind { val  = $2, expr = [ETerm $4], pos = CL.pos $1 } }
        | def Val '{' Exprs2 '}' { Bind { val  = $2, expr = $4, pos = CL.pos $1 } }

Fun :: { Expr }
Fun     : def Lower '(' Args ')' ':' Upper '=' Term ';'
        { Fun { fun = $2 , args  = $4, rettype = $7, expr  = [ETerm $9], pos = CL.pos $1 } }
        | def Lower '(' Args ')' ':' Upper '{' Exprs2 '}'
        { Fun { fun = $2 , args  = $4, rettype = $7, expr  = $9, pos = CL.pos $1 } }

Terms   :: { Term } 
Terms   : Term ';' Expr2        { SemiColon {term = $1, texpr = $3} }
        | Term                  { $1 }

Term   :: { Term } 
Term    : if Term '{' Exprs2 '}' else '{' Exprs2 '}' 
        {If {cond = $2, texprs = $4, texprs' = $8 } }
        | Lower '(' CallArgs ')'    { Call { label = $1, targs = $3 } }
        | '(' Term ')'              { $2 }
        | '{' Terms '}'             { $2 }
        | str                       { Str' (Str (CL.text $1) (CL.pos $1)) }
        | Term Op Term              { Op { op = $2, term = $1, term' = $3 } }
        | Num                       { TInt $1 }
        | Lower                     { Var $1 }

-----------------------------------------------------------------------------

Args :: { [Val] }
Args    : Val ',' Args          { $1 : $3 }
        | Val                   { [$1] }
        |                       { [] }
 
Val :: { Val }
Val     : Lower ':' Upper          { Val { name  = $1, type' = $3} }

CallArgs :: { [Term] }
CallArgs : Term ',' CallArgs      { $1 : $3 }
         | Term                   { [$1] }
         |                        { [] }

-----------------------------------------------------------------------------

Num  :: { Num } 
Num     : num   { Num (CL.num $1) (CL.pos $1) }

Str  :: { Str } 
Str     : Lower { $1 }
        | Upper { $1 }
        | Op    { $1 } 

Op  :: { Str } 
Op      : op    { Str (CL.text $1) (CL.pos $1) } 
        | var   {% fail $ "Require operator: " ++ show (CL.pos $1) }
        | type  {% fail $ "Require operator: " ++ show (CL.pos $1) }

Lower  :: { Str } 
Lower   : var   { Str (CL.text $1) (CL.pos $1) } 
        | type  {% fail $ "Require upper case: " ++ show (CL.pos $1) }
        | op    {% fail $ "Require upper case: " ++ show (CL.pos $1) }

Upper  :: { Str } 
Upper   : type  { Str (CL.text $1) (CL.pos $1) } 
        | var   {% fail $ "Require lower case: " ++ show (CL.pos $1) }
        | op    {% fail $ "Require upper case: " ++ show (CL.pos $1) }
{

data Str = Str Text CL.AlexPosn
    deriving Eq

data Num   = Num Int CL.AlexPosn
    deriving Eq

data Term
    = TInt      { num   :: Num }                                     -- 整数
    | Var       { label :: Str }                                     -- 名前
    | Str'      { text  :: Str }                                     -- 名前
    | Op        { op    :: Str, term :: Term,    term' :: Term }     -- 演算子
    | Call      { label :: Str, targs :: [Term] }                    -- Call
    | SemiColon { term  :: Term, texpr :: Expr }                     -- 連結
    | If        { cond  :: Term, texprs :: [Expr], texprs' :: [Expr] } -- if式
    deriving Eq

data Expr =
      Bind { val :: Val, expr :: [Expr], pos :: CL.AlexPosn  }
    | Fun  { fun  :: Str, args  :: [Val], rettype :: Str, expr :: [Expr], pos ::CL.AlexPosn }
    | ETerm Term
    deriving Eq

data Val = Val { name :: Str, type' :: Str }
    deriving Eq

parseError :: [CL.Token] -> a
parseError (t:ts) = error . ("Parse error - line: " ++) $ show t

parser :: [CL.Token] -> Either Text [Expr]
parser tokens = lawParser tokens

instance Show Str where
    show (Str text _pos) = unpack text

instance Show Num where
    show (Num n _pos) = show n

instance Show Val where
    show (Val n _pos) = show n

instance Show Term where
    show (TInt n)         = show n
    show (Var l)          = show l
    show (Str' t)         = show t
    show (Op op t t')     = show t ++ " " ++ show op ++ " " ++ show t'
    show (Call l  as)     = show l ++ "(" ++ (drop 2 . concat $ map (\a -> ", " ++ show a) as) ++ ")" 
    show (TInt n)         = show n
    show (SemiColon t t') = show t ++ ";\n" ++ show t'
    show (If c e e')      = "if " ++ show c ++ " {\n" ++ (addIndent . unlines $ map show e) 
                            ++ "} else {\n" ++ (addIndent . unlines $ map show e') ++ "}"

instance Show Expr where
    show (ETerm t)          = show t
    show (Bind v es _p)     = "def " ++ show v ++ " {\n"++ (addIndent . unlines $ map show es) ++ "}" 
    show (Fun v as t es _p) = "def " ++ show v ++ "(" ++ (drop 2 . concat $ map (\a -> ", " ++ show a) as) ++ "): " ++ show t 
                           ++ " {\n"++(addIndent . unlines $ map show es) ++ "}" 

addIndent = unlines . map ("\t"++) . lines
}

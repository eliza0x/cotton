{
module Cotton.Parser where

import Data.Text (Text(..))
import Cotton.Lexer hiding (If, Op, Var)
import qualified Cotton.Lexer as CL
}

%name lawParser
%tokentype { Token }
%error { parseError }
%monad { Either Text } -- { thenE } { return }

%token
  def       { Def        $$ }
  if        { CL.If      $$ }
  else      { Else       $$ }
  '<-'      { LArrow     $$ }
  '->'      { RArrow     $$ }
  '='       { Equal      $$ }
  '`'       { Backquort  $$ }
  '"'       { Quort      $$ }
  '\''      { Apostrophe $$ }
  ';'       { Semicolon  $$ }
  ':'       { Colon      $$ }
  ','       { Comma      $$ }
  '('       { LParen     $$ }
  ')'       { RParen     $$ }
  '{'       { LBrace     $$ }
  '}'       { RBrace     $$ }
  '['       { LBracket   $$ }
  ']'       { RBracket   $$ }
  num       { Num        $$ }
  op        { CL.Op      $$ }
  var       { CL.Var     $$ }
  type      { Type       $$ }

%%

Exprs :: { [Expr] }
Exprs   : Expr Exprs            { $1 : $2 }
        | Expr                  { [$1] }

Expr :: { Expr }
Expr    : Bind                  { $1 }
        | Fun                   { $1 }
        | Terms                {% fail $ "Top-level declaration expected:" ++ show $1 }

Exprs2 :: { [Expr] }
Exprs2   : Expr2 Exprs2         { $1 : $2 }
         | Expr2                { [$1] }

Expr2 :: { Expr }
Expr2   : Bind                  { $1 }
        | Fun                   { $1 }
        | Terms                 { ETerm $1 }


Bind :: { Expr }
Bind    : def Val '=' Term       { Bind { val  = $2, expr = [ETerm $4] } }
        | def Val '{' Exprs2 '}' { Bind { val  = $2, expr = $4 } }

Fun :: { Expr }
Fun     : def Lower '(' Args ')' ':' Upper '=' Term
        { Fun { ename  = fst $2 , args  = $4, rettype = (fst $7), expr  = [ETerm $9] } }
        | def Lower '(' Args ')' ':' Upper '{' Exprs2 '}'
        { Fun { ename  = fst $2 , args  = $4, rettype = (fst $7), expr  = $9 } }

Args :: { [Val] }
Args    : Val ',' Args          { $1 : $3 }
        | Val                   { [$1] }
        |                       { [] }
 
Val :: { Val }
Val     : Lower ':' Upper          { Val { name  = fst $1, type' = (fst $3) } }

CallArgs :: { [Term] }
CallArgs : Term ',' CallArgs      { $1 : $3 }
         | Term                   { [$1] }
         |                        { [] }

Terms   :: { Term } 
Terms   : Term ';' Terms        { SemiColon {term = $1, term' = $3} }
        | Term                  { $1 }

Term   :: { Term } 
Term    : if Term '{' Exprs2 '}' else '{' Exprs2 '}' 
        {If {cond = $2, texpr = $4, texpr' = $8 } }
        | Lower '(' CallArgs ')'      { Call { tname = fst $1, targs = $3 } }
        | '(' Term ')'          { $2 }
        | '{' Terms '}'         { $2 }
        | '"' Lower '"'           { Str (fst $2) }
        | '"' Upper '"'          { Str (fst $2) }
        | Term op Term          { Op { op = fst $2, term = $1, term' = $3 } }
        | num                   { TInt (fst $1) }
        | Lower                   { Var (fst $1) }

Lower   : var   { $1 }
        | type  {% fail $ "Require upper case: " ++ show $1 }

Upper   : type  { $1 }
        | var   {% fail $ "Require lower case: " ++ show $1 }


{
data Term = 
      TInt Int                                               -- 整数
    | Var Text                                               -- 名前
    | Str Text                                               -- 名前
    | If { cond :: Term, texpr :: [Expr], texpr' :: [Expr] } -- if式
    | Op { op:: Text, term :: Term, term' :: Term }          -- 演算子
    | Call { tname :: Text, targs :: [Term] }                -- Call
    | SemiColon { term :: Term, term' :: Term }              -- 連結
    deriving (Show, Eq)

data Expr =
    -- 変数定義
      Bind { val :: Val, expr :: [Expr] }
    -- 関数定義
    | Fun  { ename  :: Text, args  :: [Val], rettype :: Text, expr :: [Expr] }
    | ETerm Term
    deriving (Show, Eq)

data Val = Val { name :: Text, type' :: Text }
    deriving (Show, Eq)

parseError :: [Token] -> a
parseError (t:ts) = error . ("Parse error - line: " ++) $ show t

parser :: [Token] -> Either Text [Expr]
parser tokens = lawParser tokens
}

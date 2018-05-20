{
module Cotton.Parser where

import Data.Text (Text(..))
import Cotton.Lexer hiding (If, Op, Var)
import qualified Cotton.Lexer as CL

import Data.Extensible
import Data.Text (Text(..))
}

%name lawParser
%tokentype { Token }
%error { parseError }

%token
  def       { Def }
  if        { CL.If }
  else      { Else }
  '<-'      { LArrow }
  '->'      { RArrow }
  '='       { Equal }
  '`'       { Backquort }
  '"'       { Quort }
  '\''      { Apostrophe }
  ';'       { Semicolon }
  ':'       { Colon }
  ','       { Comma }
  '('       { LParen }
  ')'       { RParen }
  '{'       { LBrace }
  '}'       { RBrace }
  '['       { LBracket }
  ']'       { RBracket }
  num       { Num  $$ }
  op        { CL.Op   $$ }
  var       { CL.Var $$ }
  type      { Type $$ }

%%

Exprs   : Expr Exprs            { $1 : $2 }
        | Expr                  { [$1] }

Expr    : Bind                  { $1 }
        | Fun                   { $1 }
        | Terms                 { ETerm $1 }

Bind    : def Val '=' Term      { Bind { val  = $2, expr = [ETerm $4] } }
        | def Val '{' Exprs '}' { Bind { val  = $2, expr = $4 } }

Fun     : def var '(' Args ')' ':' type '=' Term
        { Fun { ename  = $2 , args  = $4, rettype = $7, expr  = [ETerm $9] } }
        | def var '(' Args ')' ':' type '{' Exprs '}'
        { Fun { ename  = $2 , args  = $4, rettype = $7, expr  = $9 } }

Args    : Val ',' Args          { $1 : $3 }
        | Val                   { [$1] }
        |                       { [] }
 
Val     : var ':' type          { Val { name  = $1, type' = $3 } }

CallArgs: Term ',' CallArgs      { $1 : $3 }
        | Term                   { [$1] }
        |                        { [] }

Terms   : Term ';' Terms        { SemiColon {term = $1, term' = $3} }
        | Term                  { $1 }

Term    : if Term '{' Exprs '}' else '{' Exprs '}' 
        {If {cond = $2, texpr = $4, texpr' = $8 } }
        | var '(' CallArgs ')'      { Call { tname = $1, targs = $3 } }
        | '(' Term ')'          { $2 }
        | '{' Terms '}'         { $2 }
        | '"' var '"'           { Str $2 }
        | '"' type '"'          { Str $2 }
        | Term op Term          { Op { op = $2, term = $1, term' = $3 } }
        | num                   { TInt $1 }
        | var                   { Var $1 }

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
parseError e = error $ "Parse error: " ++ show e

parser :: [Token] -> [Expr]
parser = lawParser
}

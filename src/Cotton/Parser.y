{
module Cotton.Parser where

import Data.Text (Text(..))
import Cotton.Lexer

import Data.Extensible
import Control.Lens hiding ((:>))
import Data.Text (Text(..))
}

%name lawParser
%tokentype { Token }
%error { parseError }

%token
  def       { Def }
  if        { If }
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
  op        { Op   $$ }
  var       { Var $$ }
  type      { Type $$ }

%%

Exprs   : Expr Exprs            { $1 : $2 }
        | Expr                  { [$1] }

Expr    : Bind                  { $1 }
        | Fun                   { $1 }
        | Terms                 { ETerm $1 }

Bind    : def Val '=' Term      { Bind { val  = $2, expr = [ETerm $4] } }
        | def Val '{' Exprs '}' { Bind { val  = $2, expr = $4 } }

Fun     : def var '(' Args ')' ':' type '{' Exprs '}'
        { Fun { ename  = $2 , args  = $4, rettype = $7, expr  = $9 } }

Args    : Val ',' Args          { $1 : $3 }
        | Val                   { [$1] }
 
Val     : var ':' type          { Val { name  = $1, type' = $3 } }

Terms   : Term ';' Terms        { SemiColon {texpr = $1, texpr' = $3} }
        | Term                  { $1 }

Term    : num                   { TInt $1 }

{
data Term = 
      TInt Int                                  -- 整数
    | Name { tname :: Text }                    -- 名前
    | Add { term :: Term, term' :: Term }       -- 加算
    | Call { tname :: Text, targs :: [Text] }    -- Call
    | SemiColon { texpr :: Term, texpr' :: Term } -- 連結
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

data Pos = Pos { filename :: Text, column :: Int, row :: Int }
    deriving (Show, Eq)

parseError :: [Token] -> a
parseError e = error $ "Parse error: " ++ show e

parser :: [Token] -> [Expr]
parser = lawParser
}

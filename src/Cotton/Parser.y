{
{-|
Module      : Cotton.Parser
Description : parser
Copyright   : (c) Sohei Yamaga, 2018
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental
Portability : POSIX

トークン列を解析して、構文木に変換します。

-}
module Cotton.Parser (
    parser,
    Stmt(..),
    Term(..),
    Arg(..),
    ) where

import Prelude hiding (Num)
import Data.Text (Text(..), unpack)
import qualified Cotton.Lexer as CL
import qualified Cotton.Type.Type as T
import Cotton.Stmt
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
  '<'       { CL.LAngle     $$ }
  '>'       { CL.RAngle     $$ }
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
  lower     { CL.Lower   $$ }
  upper     { CL.Upper   $$ }
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
        | Terms Stmts2           { term # fst $1 : $2 }
        | Terms                  { [term # fst $1] }
        | Bind                   {% fail $ "statement must be finish term." ++ show (snd $1) }
        | Fun                    {% fail $ "statement must be finish term." ++ show (snd $1) }

Bind :: { (Stmt, CL.AlexPosn) }
Bind    : def Lower ':' Type '=' Term ';'   
        { (Bind { label = fst $2, type' = fst $4, stmt = [ETerm $ fst $6], pos = CL.pos $1 }, CL.pos $1) }
        | def Lower ':' Type '{' Stmts2 '}' 
        { (Bind { label = fst $2, type' = fst $4, stmt = $6,               pos = CL.pos $1 }, CL.pos $1) }

Fun :: { (Stmt, CL.AlexPosn) }
Fun     : def Lower '(' Args ')' ':' Type '=' Term ';'
        { (Fun { label = fst $2 , args = $4, type' = fst $7, stmt = [ETerm $ fst $9], pos = CL.pos $1 }, CL.pos $1) }
        | def Lower '(' Args ')' ':' Type '{' Stmts2 '}'
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
Arg     : Lower ':' Type          { Arg { argName  = fst $1, type'' = Just (fst $3), apos = snd $1} }
        | Lower                    { Arg { argName  = fst $1, type'' = Nothing,       apos = snd $1} }

-----------------------------------------------------------------------------

-- | 整数
Num :: { (Int, CL.AlexPosn) } 
Num     : num   { (CL.num $1, CL.pos $1) }

-- | 演算子
Op :: { (Text, CL.AlexPosn) } 
Op      : op    { (CL.text $1, CL.pos $1) } 
        | lower {% fail $ "Require operator: " ++ show (CL.pos $1) }
        | upper {% fail $ "Require operator: " ++ show (CL.pos $1) }

-- | 関数、変数
Lower :: { (Text, CL.AlexPosn) } 
Lower   : lower { (CL.text $1, CL.pos $1) } 
        | upper {% fail $ "Require upper case: " ++ show (CL.pos $1) }
        | op    {% fail $ "Require upper case: " ++ show (CL.pos $1) }

-- | 型
Type :: { (T.Type, CL.AlexPosn) } 
Type   : upper '<' Type '>'      { (T.Ref $ fst $3, CL.pos $1) } 
       | '(' Types ')' '->' Type { (T.Func $2 (fst $5), snd $5) } 
       | upper                   { (T.Type $ CL.text $1, CL.pos $1) } 
       | lower                   { (T.TypeVar $ CL.text $1, CL.pos $1) }
       | op                      {% fail $ "Require upper case: " ++ show (CL.pos $1) }

Types :: { [T.Type] }
Types  : Type ',' Types     { fst $1 : $3 }
       | Type               { [fst $1] }
       |                    { [] }

-----------------------------------------------------------------------------

{
parseError :: [CL.Token] -> a
parseError ts = error $ "Parse error - line: " ++ show ts

parser :: [CL.Token] -> Either Text [Stmt]
parser tokens = lawParser tokens
}

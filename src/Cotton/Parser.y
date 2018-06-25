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
{-# LANGUAGE OverloadedStrings, FlexibleContexts, TemplateHaskell, LambdaCase, OverloadedLabels, ScopedTypeVariables  #-}

module Cotton.Parser (
    parser,
    Stmt(..),
    Term(..),
    Arg(..),
    ) where

import Data.Extensible
import Control.Lens ((#))

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
  then      { CL.Then       $$ }
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
  num       { CL.Num        $$ }
  op        { CL.Op         $$ }
  lower     { CL.Lower      $$ }
  upper     { CL.Upper      $$ }
  str       { CL.Str        $$ }

%%

-----------------------------------------------------------------------------

Stmts   :: { [Stmt] } 
Stmts   : Stmt ';' Stmts { $1 : $3 }
        | Stmt           { [$1] }
        |                { [] }

Stmt :: { Stmt }
Stmt   : def Lower ':' Type '=' Term 
       { Stmt $ (#bind     # (#name @= fst $2                <: #type @= fst $4 <: #term @= $6 <: #pos @= CL.pos $1 <: nil))}
       | def Lower '(' Args ')' ':' Type '=' Term
       { Stmt $ (#function # (#name @= fst $2 <: #args @= $4 <: #type @= fst $7 <: #term @= $9 <: #pos @= CL.pos $1 <: nil))}
       | Term           
       { Stmt $ #term # $1 }

-- | 関数定義時の引数リスト
Args :: { [Arg] }
Args    : Arg ',' Args          { $1 : $3 }
        | Arg                   { [$1] }
        |                       { [] }
 
Arg :: { Arg }
Arg     : Lower ':' Type        { #name @= fst $1 <: #type @= Just (fst $3) <: #pos @= snd $1 <: nil }
        | Lower                 { #name @= fst $1 <: #type @= Nothing       <: #pos @= snd $1 <: nil }

-----------------------------------------------------------------------------

Term   :: { Term } 
Term    : if Term then Term else Term 
                              { Term $ (#if # (#cond @= $2 <: #then @= $4 <: #else @= $6 <: #pos @= CL.pos $1 <: nil)) }
        | Lower '<-' Term     { Term $ #overwrite # (#name @= fst $1 <: #type @= Nothing <: #term @= $3 <: #pos @= CL.pos $2 <: nil) }
        | Lower '(' Calls ')' { Term $ #call # (#name @= fst $1 <: #args @= $3 <: #pos @= snd $1 <: nil) }
        | '(' Term ')'        { $2 }
        | '{' Stmts '}'       { Term $ #stmts # $2 }
        | str                 { Term $ #str # (#text @= CL.text $1 <: #pos @= CL.pos $1 <: nil) }
        | Term Op Term        { Term $ (#call # (#name @= fst $2 <: #args @= [$1, $3] <: #pos @= snd $2 <: nil)) }
        | Num                 { Term $ (#nat # (#num  @= fst $1 <: #pos @= snd $1 <: nil)) }
        | Lower               { Term $ (#var # (#name @= fst $1 <: #pos @= snd $1 <: nil)) }

-- | 関数呼び出し時の引数リスト
Calls :: { [Term] }
Calls   : Term ',' Calls        { $1 : $3 }
        | Term                  { [$1] }
        |                       { [] }
 
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

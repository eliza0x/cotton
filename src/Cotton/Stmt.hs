{-# LANGUAGE DataKinds, TypeOperators, OverloadedLabels, FlexibleContexts, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Cotton.Stmt where

import qualified Cotton.Lexer as CL
import Data.Extensible
import Control.Lens hiding ((:>))
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Cotton.Type.Type
import qualified Data.Text as T

-- | 文
newtype Stmt = Stmt { stmt :: StmtBase }
type StmtBase = Variant 
    '[ "bind"      :> Bind
     , "function"  :> Fun
     , "term"      :> Term 
     ]

type Arg = Record ('[ "name" :> Text, "type" :> Maybe Type ] ++ Pos)

type Bind = Record (
    '[ "name" :> Text
     , "type" :> Type
     , "term" :> Term
     ] ++ Pos)

type Fun = Record (
    '[ "name" :> Text
     , "args"  :> [Arg]
     , "type"  :> Type
     , "term"  :> Term
     ] ++ Pos)

newtype Term = Term { term :: TermBase }
type TermBase = Variant
    '[ "nat"       :> Nat
     , "var"       :> Var
     , "str"       :> Str
     , "overwrite" :> Overwrite
     , "call"      :> Call
     , "if"        :> If
     , "stmts"     :> [Stmt]
     ]

type Nat       = Record ('[ "num"   :> Int                                          ] ++ Pos)
type Var       = Record ('[ "name"  :> Text                                         ] ++ Pos)
type Str       = Record ('[ "text"  :> Text                                         ] ++ Pos)
type Overwrite = Record ('[ "name"  :> Text, "type"  :> Maybe Type, "term"  :> Term ] ++ Pos)
type Call      = Record ('[ "name"  :> Text, "args"  :> [Term]                      ] ++ Pos)
type If        = Record ('[ "cond"  :> Term, "then"  :> Term,       "else"  :> Term ] ++ Pos)

type Pos = '[ "pos" :> CL.AlexPosn ]

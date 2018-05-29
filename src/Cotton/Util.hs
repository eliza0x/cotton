{-|
Module      : Cotton.Util
Description : Utility
Copyright   : (c) Sohei Yamaga, 2018
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts, OverloadedLabels, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Cotton.Util where

import Data.Extensible
import Control.Lens hiding ((:>))
import Data.Monoid ((<>))
import Data.Text (Text)
import Cotton.Type.Type
import Cotton.Lexer

mkField "name type' pos"
type Var = Record
    [ "name"  :> Text
    , "type'" :> Type
    , "pos"   :> Maybe AlexPosn
    ]        

mkField "value"
type Number = Record 
    [ "value" :> Int
    , "pos" :> Maybe AlexPosn
    ]

mkField "text"
type Str = Record 
    [ "text" :> Text
    , "pos"  :> Maybe AlexPosn]

data Null = Null
    deriving (Show, Eq)

type RegField = 
    '[ "globalReg" >: Var 
    ] ++ ValField

type ValField =
    '[ "reg"       >: Var
    ,  "int"       >: Number
    ,  "str"       >: Str
    ,  "bool"      >: Bool
    ,  "null"      >: Null
    ]


mkField "globalReg reg int str bool null"
type Reg = Variant (RegField)

isVar :: Reg -> Bool
isVar = matchField
    $ #globalReg @= const True
   <: #reg       @= const True
   <: #int       @= const False
   <: #str       @= const False
   <: #bool      @= const False
   <: #null      @= const False
   <: nil

isNull :: Reg -> Bool
isNull = matchField
    $ #globalReg @= const False
   <: #reg       @= const False
   <: #int       @= const False
   <: #str       @= const False
   <: #bool      @= const False
   <: #null      @= const True
   <: nil

nameOf :: Reg -> Text
nameOf = matchField
          $ #globalReg @= (\r -> "@" <> r ^. #name)
         <: #reg       @= (\r -> "%" <> r ^. #name)
         <: #int       @= error "int"
         <: #str       @= error "str"
         <: #bool      @= error "bool"
         <: #null      @= error "null"
         <: nil

typeOf :: Reg -> Type
typeOf = matchField
          $ #globalReg @= (\r -> r ^. #type')
         <: #reg       @= (\r -> r ^. #type')
         <: #int       @= const (Type "I32")
         <: #str       @= const (Type "String")
         <: #bool      @= const (Type "Bool")
         <: #null      @= error "null"
         <: nil

addIndent :: String -> String
addIndent = unlines . map ("\t"++) . lines


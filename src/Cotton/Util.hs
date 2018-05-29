{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts, OverloadedLabels, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Cotton.Util where

import Data.Extensible
import Control.Lens hiding ((:>))

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

mkField "var null num str"
type Val = Variant
    [ "var"  >: Var
    , "num"  >: Number
    , "str"  >: Str
    , "null" >: Null
    ]

valType :: Val -> Type
valType = matchField
    $ #var  @= (\r -> r ^. #type')
   <: #num  @= (\_ -> Type "I32")
   <: #str  @= (\_ -> Type "String")
   <: #null @= const Bottom
   <: nil

valName :: Val -> Text
valName = matchField
    $ #var  @= (\r -> r ^. #name)
   <: #num  @= error "error num"
   <: #str  @= error "error str"
   <: #null @= error "error null"
   <: nil

isVar :: Val -> Bool
isVar = matchField
    $ #var  @= const True
   <: #num  @= const False
   <: #str  @= const False
   <: #null @= const False
   <: nil

isNull :: Val -> Bool
isNull = matchField
    $ #var  @= const False
   <: #num  @= const False
   <: #str  @= const False
   <: #null @= const True
   <: nil


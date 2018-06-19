{-|
Module      : Cotton.Util
Description : Utility
Copyright   : (c) Sohei Yamaga, 2018
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedLabels, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Cotton.Util where

import Data.Extensible
import Control.Lens hiding ((:>))
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Cotton.Type.Type
import Cotton.Lexer

type Var = Record
    [ "name" :> Text
    , "type" :> Type
    , "pos"  :> Maybe AlexPosn
    ]        

type Number = Record 
    [ "value" :> Int
    , "pos" :> Maybe AlexPosn
    ]

type Str = Record 
    [ "text" :> Text
    , "pos"  :> Maybe AlexPosn]

data Null = Null
    deriving (Show, Eq)

type ValField = 
    '[ "var"  >: Var 
     ]

type RegField = 
    '[ "globalReg" >: Var 
     , "reg"       >: Var
     ]

type BaseField =
    '[ "int"  >: Number
     , "str"  >: Str
     , "bool" >: Bool
     , "null" >: Null
     ]

type Reg = Variant (RegField ++ BaseField)
type Val = Variant (ValField ++ BaseField)

showBase :: IncludeAssoc (ValField ++ RegField ++ BaseField) xs => Variant xs -> Text
showBase = matchField $ shrinkAssoc
          $ #var       @= (\(r :: Var   ) -> r ^. #name)
         <: #globalReg @= (\(r :: Var   ) -> "@" <> r ^. #name)
         <: #reg       @= (\(r :: Var   ) -> "%" <> r ^. #name)
         <: #int       @= (\(r :: Number) -> pack . show $ r ^. #value )
         <: #str       @= (\(r :: Str   ) -> pack . show $ r ^. #text )
         <: #bool      @= (\(r :: Bool  ) -> pack $ show r )
         <: #null      @= (\(r :: Null  ) -> pack $ show r )
         <: nil

isVar :: IncludeAssoc (ValField ++ RegField ++ BaseField) xs => Variant xs -> Bool
isVar = matchField $ shrinkAssoc
    $ #var       @= (\(_ :: Var   ) -> True)
   <: #globalReg @= (\(_ :: Var   ) -> True)
   <: #reg       @= (\(_ :: Var   ) -> True)
   <: #int       @= (\(_ :: Number) -> False)
   <: #str       @= (\(_ :: Str   ) -> False)
   <: #bool      @= (\(_ :: Bool  ) -> False)
   <: #null      @= (\(_ :: Null  ) -> False)
   <: nil

isNull :: IncludeAssoc (ValField ++ RegField ++ BaseField) xs => Variant xs -> Bool
isNull = matchField $ shrinkAssoc
    $ #var       @= (\(_ :: Var   ) -> False)
   <: #globalReg @= (\(_ :: Var   ) -> False)  
   <: #reg       @= (\(_ :: Var   ) -> False)  
   <: #int       @= (\(_ :: Number) -> False) 
   <: #str       @= (\(_ :: Str   ) -> False) 
   <: #bool      @= (\(_ :: Bool  ) -> False) 
   <: #null      @= (\(_ :: Null  ) -> True) 
   <: nil

nameOf :: IncludeAssoc (ValField ++ RegField ++ BaseField) xs => Variant xs -> Text
nameOf = matchField $ shrinkAssoc
          $ #var       @= (\(r :: Var   ) -> r ^. #name)
         <: #globalReg @= (\(r :: Var   ) -> r ^. #name)
         <: #reg       @= (\(r :: Var   ) -> r ^. #name)
         <: #int       @= (\(r :: Number) -> error ("int: "  ++ show r))
         <: #str       @= (\(r :: Str   ) -> error ("str: "  ++ show r))
         <: #bool      @= (\(r :: Bool  ) -> error ("bool: " ++ show r) )
         <: #null      @= (\(r :: Null  ) -> error ("null: " ++ show r) )
         <: nil

typeOf :: IncludeAssoc (ValField ++ RegField ++ BaseField) xs => Variant xs -> Type
typeOf = matchField $ shrinkAssoc
          $ #var       @= (\(r :: Var   ) -> r ^. #type   )
         <: #globalReg @= (\(r :: Var   ) -> r ^. #type   )
         <: #reg       @= (\(r :: Var   ) -> r ^. #type   )
         <: #int       @= (\(_ :: Number) -> Type "I32"   )
         <: #str       @= (\(_ :: Str   ) -> Type "String")
         <: #bool      @= (\(_ :: Bool  ) -> Type "Bool"  )
         <: #null      @= (\(_ :: Null  ) -> Bottom       )
         <: nil

val2Reg :: Val -> Reg
val2Reg = matchField 
     $ #var  @= (\r -> #reg  # r)
    <: #int  @= (\r -> #int  # r)
    <: #str  @= (\r -> #str  # r)
    <: #bool @= (\r -> #bool # r)
    <: #null @= (\r -> #null # r)
    <: nil

addIndent :: String -> String
addIndent = unlines . map ("\t"++) . lines


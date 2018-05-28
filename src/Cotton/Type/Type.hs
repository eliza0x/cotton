{-# LANGUAGE OverloadedStrings #-}

module Cotton.Type.Type where

import Data.Text (Text, unpack)

data Type 
    = Type Text
    | Func [Type] Type
    | Ref Type
    | TypeVar Text
    | Bottom
    deriving Eq

instance Show Type where
    show (Type "I32")  = "i32"
    show (Type "Bool") = "i1"
    show (Type "Unit") = "void"

    show (Type t)    = unpack t
    show (TypeVar t) = unpack t
    show (Func as t) = "("++drop 2 (concatMap (\a -> ", " ++ show a) as)++"): "++show t
    show (Ref a)     = show a++"*"
    show Bottom      = "_|_"

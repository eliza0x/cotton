{-|
Module      : Cotton.LLVM
Description : generate LLVM IR
Copyright   : (c) Sohei Yamaga, 2018
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental
Portability : POSIX

K正規化された式からLLVM IRを生成します。

-}

{-# LANGUAGE OverloadedStrings #-}

module Cotton.LLVM where

import qualified Cotton.KNormalize as K
import qualified Data.ByteString.Char8 as BC8

import qualified LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module


toMod :: [K.Block] -> AST.Module
toMod blocks = undefined

toLLVM :: [K.Block] -> IO BC8.ByteString
toLLVM blocks = undefined

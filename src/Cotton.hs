module Cotton where

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Control.Monad as C
import qualified Data.ByteString.Char8 as BC8

{-
import Cotton.Lexer
import Cotton.Parser
import Cotton.KNormalize
import Cotton.Closure
import Cotton.Alpha
import Cotton.Type
import Cotton.LLVM
-}

-- | 文字列を投げ込めばいい感じにやってくれる
compile :: Bool -> String -> IO BC8.ByteString
compile isDebugMode sourceCode = do
	{-
    let token = lexer sourceCode :: [Token]
    (put . show) token
    put "\n==== 構文解析 ===="
    case parser token of
        Left err -> error $ "error: " ++ show err
        Right ts -> do
            mapM_ (put . show) ts
            put "\n==== Alpha変換 ===="
            let ts' = alpha ts
            mapM_ (put . show) ts'
            put "\n==== 型検査 ===="
            let typeEnv = typeCheck ts'
            (put . show) typeEnv
            put "\n==== 暗黙引数検査 ===="
            let implicitArgs = inspectImplicitArgs ts'
            (put . show) implicitArgs
            put "\n==== Clojure変換 ===="
            let cts = closure typeEnv ts'
            mapM_ (put . show) cts
            let typeEnv' = appendImplicitArgs typeEnv implicitArgs
            put "\n==== K正規化 ===="
            knorm <- knormalize typeEnv' cts
            mapM_ (put . show) knorm
            put "\n==== LLVM IR ===="
            llvmir <- toLLVM knorm
            put $ BC8.unpack llvmir
            return llvmir
			-}
			undefined
    where put  t = C.when isDebugMode $ putStrLn t

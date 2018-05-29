module Cotton where

import Data.Text (Text(..))
import qualified Data.Text as T
import Control.Monad

import Cotton.Parser
import Cotton.Lexer
import Cotton.KNormalize
import Cotton.Closure
import Cotton.Alpha
import Cotton.Type
import Cotton.LLVM
import Cotton.Util

-- | 文字列を投げ込めばいい感じにやってくれる
compile :: String -> IO Text
compile sourceCode = do
    let token = lexer sourceCode :: [Token]
    print token
    putStrLn "\n=========="
    putStrLn "構文解析\n"
    case parser token of
        Left err -> error $ "error: " ++ show err
        Right ts -> do
            mapM_ print ts
            putStrLn "\n=========="
            putStrLn "Alpha変換\n"
            let ts' = alpha ts
            mapM_ print ts'
            putStrLn "\n=========="
            putStrLn "型検査\n"
            let typeEnv = typeCheck ts'
            print typeEnv
            putStrLn "\n=========="
            putStrLn "暗黙引数検査\n"
            let implicitArgs = inspectImplicitArgs ts'
            print implicitArgs
            putStrLn "\n=========="
            putStrLn "Clojure変換\n"
            let cts = closure typeEnv ts'
            mapM_ print cts
            let typeEnv' = appendImplicitArgs typeEnv implicitArgs
            putStrLn "\n=========="
            putStrLn "K正規化\n"
            knorm <- knormalize typeEnv' cts
            mapM_ print knorm
            putStrLn "\n=========="
            putStrLn "LLVM IR\n"
            return ""
            llvmir <- knorm2llvmir knorm
            let text = toText llvmir
            return text


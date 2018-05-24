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

-- | 文字列を投げ込めばいい感じにやってくれる
compile :: String -> IO String
compile sourceCode = do
    let token = lexer sourceCode :: [Token]
    -- print token
    -- putStrLn "\n=========="
    -- putStrLn "構文解析\n"
    case parser token of
        Left err -> error $ "error: " ++ show err
        Right ts -> do
            -- mapM_ print ts
            -- putStrLn "\n=========="
            -- putStrLn "Alpha変換\n"
            let ts' = alpha ts
            -- mapM_ print ts'
            -- putStrLn "\n=========="
            -- putStrLn "型検査\n"
            let typeEnv = typeCheck ts'
            -- print typeEnv
            -- putStrLn "\n=========="
            -- putStrLn "暗黙引数検査\n"
            let its' = inspectImplicitArgs ts'
            -- print its'
            -- putStrLn "\n=========="
            -- putStrLn "Clojure変換\n"
            let cts = closure typeEnv ts'
            -- mapM_ print cts
            -- putStrLn "\n=========="
            -- putStrLn "K正規化\n"
            knorm <- knormalize typeEnv cts
            -- mapM_ print knorm
            -- putStrLn "\n=========="
            -- putStrLn "LLVM IR\n"
            llvmir <- knorm2llvmir knorm
            let asm = map show llvmir
            return $ concat asm


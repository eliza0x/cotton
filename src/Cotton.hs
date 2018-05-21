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

-- | 文字列を投げ込めばいい感じにやってくれる
compile :: String -> IO ()
compile sourceCode = do
    let token = lexer sourceCode :: [Token]
    print token
    putStrLn "\n=========="
    putStrLn "構文解析\n"
    case parser token of
        Left err -> putStrLn $ "error: " ++ show err
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
            let its' = inspectImplicitArgs ts'
            print its'
            putStrLn "\n=========="
            putStrLn "Clojure変換\n"
            let uts = closure typeEnv ts'
            mapM_ print uts


module Cotton where

import Data.Text (Text(..))
import qualified Data.Text as T
import Control.Monad

import Cotton.Parser
import Cotton.Lexer
import Cotton.KNormalize

-- | 文字列を投げ込めばいい感じにやってくれる
compile :: String -> IO ()
compile sourceCode = do
    let token = lexer sourceCode :: [Token]
    print token
    putStrLn "\n==========\n"
    case parser token of
        Left err -> putStrLn $ "error: " ++ show err
        Right ts -> do
            mapM_ print ts
            let ts' = knormalize ts
            putStrLn "\n==========\n"
            mapM_ print ts'
            let its' = inspectImplicitArgs ts
            putStrLn "\n==========\n"
            print its'


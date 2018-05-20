module Cotton where

import Data.Text (Text(..))
import qualified Data.Text as T
import Control.Monad

import Cotton.Parser
import Cotton.Lexer

-- | 文字列を投げ込めばいい感じにやってくれる
compile :: String -> IO ()
compile sourceCode = do
    let token = lexer sourceCode :: [Token]
    print token
    putStrLn "\n==========\n"
    case parser token of
        Right ts -> mapM_ print ts
        Left err -> print err


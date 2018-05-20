module Cotton where

import Data.Text (Text(..))
import qualified Data.Text as T
import Control.Monad

import Cotton.Parser
import Cotton.Lexer

compile :: String -> IO ()
compile sourceCode = do
    let token = lexer sourceCode :: [Token]
    print token
    putStrLn "\n==========\n"
    print $ parser token

    return undefined


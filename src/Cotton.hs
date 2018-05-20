module Cotton where

import Data.Text (Text(..))
import qualified Data.Text as T

import Cotton.Parser
import Cotton.Lexer

compile :: String -> IO ()
compile sourceCode = do
    let token = lexer sourceCode :: Maybe [Token]
    print token
    case token of
        Nothing    -> return ()
        Just token -> do
            putStrLn "\n==========\n"
            let st         = parser token
            print st

            putStrLn "\n==========\n"
            return undefined


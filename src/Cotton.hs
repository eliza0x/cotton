module Cotton where

import Data.Text (Text(..))
import qualified Data.Text as T
import Control.Monad

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
            mapM_ print st

            return undefined


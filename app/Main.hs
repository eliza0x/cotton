module Main where

import Cotton
import System.Environment
import Control.Monad

main :: IO ()
main = do
    args <- getArgs 
    when (length args >= 1)
        $ putStrLn =<< compile =<< readFile (args!!0)

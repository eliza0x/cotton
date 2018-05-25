module Main where

import Cotton
import System.Environment
import Control.Monad

main :: IO ()
main = do
    args <- getArgs 
    unless (null args)
        $ putStrLn =<< compile =<< readFile (args!!0)

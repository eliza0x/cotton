module Main where

import Cotton
import System.Environment
import Control.Monad
import qualified Data.Text.IO as T

main :: IO ()
main = do
    args <- getArgs 
    unless (null args)
        $ T.putStrLn =<< compile =<< readFile (args!!0)

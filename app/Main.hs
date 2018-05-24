module Main where

import Cotton

main :: IO ()
main = do
    putStrLn =<< compile =<< getContents

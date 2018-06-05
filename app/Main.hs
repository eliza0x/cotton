{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Cotton as C
import Options.Declarative
import Control.Monad.IO.Class
import qualified System.Process as Sys
import qualified System.Exit    as E
import qualified Data.Text      as T
import qualified Data.Text.IO   as T


compile :: Bool -> String -> IO ()
compile isDebugMode filepath = do
    llvmir <- C.compile isDebugMode =<< readFile filepath
    (exitCode, _, _) <- Sys.readCreateProcessWithExitCode (Sys.shell "lli") (T.unpack llvmir)
    case exitCode of
        E.ExitFailure n -> print n
        _               -> print 0
        

debugSubCmd :: Arg "FILEPATH" String
            -> Cmd "execute source code (verbose)" ()
debugSubCmd filepath = liftIO $ compile True (get filepath)

runSubCmd :: Arg "FILEPATH" String
          -> Cmd "execute source code" ()
runSubCmd filepath = liftIO $ compile False (get filepath)

main :: IO ()
main = run_ $
    Group "foo" 
    [ subCmd "run"     runSubCmd
    , subCmd "debug"   debugSubCmd
    ]


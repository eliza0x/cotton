import Test.Hspec
import Control.Lens
import qualified Cotton as C
import qualified Spec.Lexer as L
import qualified System.Exit    as Sys
import qualified System.Process as Sys
import qualified Data.Text      as T
import qualified Data.Text.IO   as T

main :: IO ()
main = hspec $ do
    L.spec
    runSimpleSpec
    runRefSpec

runRefSpec :: Spec
runRefSpec = do
    ref1 <- runIO $ runProgram "./example/ref/ref1.ctn"
    ref2 <- runIO $ runProgram "./example/ref/ref2.ctn"
    ref3 <- runIO $ runProgram "./example/ref/ref3.ctn"
    ref4 <- runIO $ runProgram "./example/ref/ref4.ctn"
    ref5 <- runIO $ runProgram "./example/ref/ref5.ctn"
    ref6 <- runIO $ runProgram "./example/ref/ref6.ctn"
    ref7 <- runIO $ runProgram "./example/ref/ref7.ctn"
    ref8 <- runIO $ runProgram "./example/ref/ref8.ctn"
    describe "run ref" $ do
        it "ref/ref1.ctn" $ ref1 == 200
        it "ref/ref2.ctn" $ ref2 == 11
        it "ref/ref3.ctn" $ ref3 == 11
        it "ref/ref4.ctn" $ ref4 == 30
        it "ref/ref5.ctn" $ ref5 == 100
        it "ref/ref6.ctn" $ ref6 == 50
        it "ref/ref7.ctn" $ ref7 == 20
        it "ref/ref8.ctn" $ ref8 == fact 5

runSimpleSpec :: Spec
runSimpleSpec = do
    add_ctn        <- runIO $ runProgram "./example/add.ctn"
    add2_ctn       <- runIO $ runProgram "./example/add2.ctn"
    closure_ctn    <- runIO $ runProgram "./example/closure.ctn"
    fact_ctn       <- runIO $ runProgram "./example/fact.ctn"
    function_ctn   <- runIO $ runProgram "./example/function.ctn"
    if_ctn         <- runIO $ runProgram "./example/if.ctn"
    nest_fact_ctn  <- runIO $ runProgram "./example/nest_fact.ctn"
    var_ctn        <- runIO $ runProgram "./example/var.ctn"
    calc_ctn       <- runIO $ runProgram "./example/calc.ctn"
    global_ctn     <- runIO $ runProgram "./example/global.ctn"
    inner_def_ctn  <- runIO $ runProgram "./example/innner_def.ctn"
    nest_def_ctn   <- runIO $ runProgram "./example/nest_def.ctn"
    multi_args_ctn <- runIO $ runProgram "./example/multi_args.ctn"
    describe "run simple" $ do
        it "add.ctn"       $ add_ctn       == 30
        it "add2.ctn"      $ add2_ctn      == 30
        it "closure_ctn"   $ closure_ctn   == 10
        it "fact.ctn"      $ fact_ctn      == fact 5
        it "function.ctn"  $ function_ctn  == 20
        it "if.ctn"        $ if_ctn        == 20
        it "nest_fact.ctn" $ nest_fact_ctn == fact 5
        it "var.ctn"       $ var_ctn       == (1 + 20) * 3
        it "calc.ctn"       $ calc_ctn       == (5 + 3) + (5 - 3) + (5 * 3) + (5 `div` 3)
        it "global.ctn"     $ global_ctn     == 100
        it "innner_def.ctn" $ inner_def_ctn  == (1 + 20) * 3
        it "nest_def.ctn"   $ nest_def_ctn   == 100
        it "multi_args.ctn" $ multi_args_ctn == 30
    
fact :: Int -> Int
fact n = if n == 0 then 1 else n * fact (n-1)

runProgram :: String -> IO Int
runProgram path = do
    source <- readFile path
    llvmir <- C.compile False source
    (exitCode, _, _) <- Sys.readCreateProcessWithExitCode (Sys.shell "lli") (T.unpack llvmir)
    return $ case exitCode of
        Sys.ExitSuccess   -> 0
        Sys.ExitFailure n -> n


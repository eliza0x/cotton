import Test.Hspec
import qualified Spec.Lexer as L

main :: IO ()
main = hspec $ do
    L.spec

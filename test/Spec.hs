import Test.Hspec
import qualified Test.LexTree as TS

main :: IO ()
main =
  hspec $ do
    TS.spec

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Test.Hspec
import qualified Test.STree as TS

main :: IO ()
main =
  hspec $ do
    TS.spec

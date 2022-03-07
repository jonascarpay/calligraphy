{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Test.Hspec
import Test.STree qualified as TS

main :: IO ()
main =
  hspec $ do
    TS.spec

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Applicative
import Control.Monad
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import STree
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance (Arbitrary a, Arbitrary p, Ord p, Num p) => Arbitrary (STree p a) where
  arbitrary = sized goSized
    where
      goSized 0 = pure Tip
      goSized n =
        oneof
          [ pure Tip,
            do
              m <- goSized (div n 2)
              (l, r) <- suchThat (liftA2 (,) (goSized (div n 2)) (goSized (div n 2))) $ \(l, r) -> abs (height l - height r) < 3
              a <- arbitrary
              (padL, padR) <- suchThatMap arbitrary $ \(NonNegative padL, NonNegative padR) -> do
                guard (padL > 0 || padL > 0)
                pure (padL, padR)
              NonNegative borderL <- arbitrary
              NonNegative borderR <- arbitrary
              let lr = maybe 0 snd (range l)
              let (ml, mr) = fromMaybe (0, 0) (range m)
                  pl = lr + borderL
                  pml = pl + padL
                  pmr = pml + mr
                  pr = pmr + padR + borderR
              pure $ bin l pl a (shift pml m) (pmr + padR) (shift pr r)
          ]
  shrink Tip = []
  shrink t@(Bin _ ls l a ms r rs) =
    concat
      [ [Tip, ls, ms, rs],
        (\ms' -> bin ls l a ms' r rs) <$> shrink ms,
        (\a' -> bin ls l a' ms r rs) <$> shrink a
      ]

main :: IO ()
main =
  hspec $
    describe "STree" $ do
      prop "arbitrary trees are valid" $ check @Int @()
      prop "inserting increases length by 1" $ \l a r (t :: STree Int Int) ->
        case insert l a r t of
          Left _ -> discard
          Right t' -> length t + 1 == length t'
      prop "inserting produces valid trees" $ \l a r (t :: STree Int Int) ->
        case insert l a r t of
          Left _ -> discard
          Right t' -> check t'
      prop "inserting leaves other elements in the same order" $ \l a r (t :: STree Int Int) ->
        let f (a : as) (b : bs) | a == b = f as bs
            f (a : as) bs = as == bs
            f _ _ = False
         in case insert l a r t of
              Left _ -> discard
              Right t' -> f (toList t') (toList t)

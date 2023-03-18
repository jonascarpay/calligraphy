{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.LexTree where

import Calligraphy.Util.LexTree
import qualified Data.Foldable as Foldable
import Data.Maybe (fromMaybe, isJust)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

#if MIN_VERSION_base(4,18,0)
import Control.Applicative ()
#else
import Control.Applicative (liftA2)
#endif

instance (Arbitrary a, Arbitrary p, Ord p, Num p) => Arbitrary (LexTree p a) where
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
              let (_ml, mr) = fromMaybe (0, 0) (range m)
                  pl = lr + borderL
                  pml = pl + padL
                  pmr = pml + mr
                  pr = pmr + padR + borderR
              pure $ bin l pl a (shift pml m) (pmr + padR) (shift pr r)
          ]
  shrink Tip = []
  shrink (Bin _ ls l a ms r rs) =
    concat
      [ [Tip, ls, ms, rs],
        (\ms' -> bin ls l a ms' r rs) <$> shrink ms,
        (\a' -> bin ls l a' ms r rs) <$> shrink a
      ]

spec :: Spec
spec =
  describe "LexTree" $ do
    prop "arbitrary trees are valid" $ check @Int @()
    prop "inserting increases length by 1" $ \l a r (t :: LexTree Int Int) ->
      case insert l a r t of
        Left _ -> discard
        Right t' -> length t + 1 == length t'
    describe "inserting produces valid trees" $ do
      prop "inserting produces trees with valid bounds" $ \l a r (t :: LexTree Int Int) ->
        case insert l a r t of
          Left _ -> discard
          Right t' -> checkBounds t'
      prop "inserting produces trees with valid height" $ \l a r (t :: LexTree Int Int) ->
        case insert l a r t of
          Left _ -> discard
          Right t' -> checkHeight t'
      prop "inserting produces trees with valid balance" $ \l a r (t :: LexTree Int Int) ->
        case insert l a r t of
          Left _ -> discard
          Right t' -> checkBalance t'
    prop "after inserting, we can get the element back" $ \l a r (t :: LexTree Int Int) ->
      case insert l a r t of
        Left _ -> discard
        Right t' -> elem a (lookupStack l t')
    prop "inserting leaves other elements in the same order" $ \l p r (t :: LexTree Int Int) ->
      let f (a : as) (b : bs) | a == b = f as bs
          f (_ : as) bs = as == bs
          f _ _ = False
       in case insert l p r t of
            Left _ -> discard
            Right t' -> f (Foldable.toList t') (Foldable.toList t)
    prop "listifying and back succeeds and is valid" $ \(t :: LexTree Int Int) ->
      case foldM (\acc (l, a, r) -> insert l a r acc) Tip (toList t) of
        Left _ -> False
        Right t' -> check t'
    prop "listifying and back succeeds and is the same tree" $ \(t :: LexTree Int Int) ->
      case foldM (\acc (l, a, r) -> insert l a r acc) Tip (toList t) of
        Left _ -> False
        Right t' -> t == t'

checkBounds :: Ord p => LexTree p a -> Bool
checkBounds t = isJust $ foldLexTreeM (pure Nothing) f t
  where
    f ml bl _ msub br mr = do
      guard (bl < br)
      forM_ msub $ \(sl, sr) -> guard $ sl >= bl && sr <= br
      l <- case ml of
        Nothing -> pure bl
        Just (ll, lr) -> do
          guard $ lr <= bl
          pure ll
      r <- case mr of
        Nothing -> pure br
        Just (rl, rr) -> do
          guard $ rl >= br
          pure rr
      pure (Just (l, r))

foldLexTreeM :: Monad m => m r -> (r -> p -> a -> r -> p -> r -> m r) -> LexTree p a -> m r
foldLexTreeM fTip fBin =
  foldLexTree
    fTip
    (\ml l a mm r mr -> do l' <- ml; m' <- mm; r' <- mr; fBin l' l a m' r r')

check :: Ord p => LexTree p a -> Bool
check t = checkBounds t && checkHeight t && checkBalance t

checkHeight :: LexTree p a -> Bool
checkHeight = isJust . go
  where
    go Tip = pure 0
    go (Bin h ml _ _ m _ mr) = do
      l <- go ml
      r <- go mr
      _ <- go m
      let h' = max l r + 1
      guard (h == h')
      pure h'

checkBalance :: LexTree p a -> Bool
checkBalance Tip = True
checkBalance (Bin _ l _ _ m _ r) =
  and
    [ checkBalance l,
      checkBalance r,
      checkBalance m,
      abs (height l - height r) < 2
    ]

range :: LexTree p a -> Maybe (p, p)
range = foldLexTree Nothing $ \l pl _ _ pr r -> Just (maybe pl fst l, maybe pr snd r)

lookupStack :: Ord p => p -> LexTree p a -> [a]
lookupStack p = foldLexTree [] f
  where
    f ls l a m r rs
      | p >= l && p < r = a : m
      | p < l = ls
      | p >= r = rs
      | otherwise = error "impossible"

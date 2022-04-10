{-# LANGUAGE DeriveTraversable #-}

module STree
  ( STree (..),
    TreeError (..),
    lookupInner,
    lookupOuter,
    insert,
    emptySTree,
    foldSTree,
    insertWith,
    height,
    toList,
    bin,
    shift,
  )
where

import Control.Applicative

data STree p a
  = Tip
  | Bin {-# UNPACK #-} !Int !(STree p a) !p a !(STree p a) !p !(STree p a)
  deriving (Show, Functor, Foldable, Traversable)

instance (Eq p, Eq a) => Eq (STree p a) where
  ta == tb = toList ta == toList tb

lookupInner :: Ord p => p -> STree p a -> Maybe a
lookupInner p = foldSTree Nothing f
  where
    f ls l a m r rs
      | p >= l && p < r = m <|> Just a
      | p < l = ls
      | p >= r = rs
      | otherwise = error "impossible"

lookupOuter :: Ord p => p -> STree p a -> Maybe a
lookupOuter p = foldSTree Nothing f
  where
    f ls l a _ r rs
      | p >= l && p < r = Just a
      | p < l = ls
      | p >= r = rs
      | otherwise = error "impossible"

toList :: STree p a -> [(p, a, p)]
toList t = foldSTree id f t []
  where
    f ls l a m r rs = ls . ((l, a, r) :) . m . rs

foldSTree :: r -> (r -> p -> a -> r -> p -> r -> r) -> STree p a -> r
foldSTree fTip fBin = go
  where
    go Tip = fTip
    go (Bin _ ls l a ms r rs) = fBin (go ls) l a (go ms) r (go rs)

emptySTree :: STree p a
emptySTree = Tip

{-# INLINE height #-}
height :: STree p a -> Int
height Tip = 0
height (Bin h _ _ _ _ _ _) = h

shift :: Num p => p -> STree p a -> STree p a
shift p = go
  where
    go Tip = Tip
    go (Bin h ls l a ms r rs) = Bin h (go ls) (l + p) a (go ms) (r + p) (go rs)

data TreeError p a
  = InvalidBounds p a p
  | OverlappingBounds a a p p
  | MidSplit
  | LexicalError p a p (STree p a)
  deriving (Functor, Foldable, Traversable, Eq, Show)

{-# INLINE bin' #-}
bin' :: STree p a -> p -> a -> STree p a -> p -> STree p a -> STree p a
bin' ls l a ms r rs = Bin (max (height ls) (height rs) + 1) ls l a ms r rs

-- | Only works if the height difference of the two trees is at most 2
{-# INLINE bin #-}
bin :: STree p a -> p -> a -> STree p a -> p -> STree p a -> STree p a
bin (Bin lh lls ll la lms lr lrs) l a ms r rs
  | lh > height rs + 1 =
    case lrs of
      Bin lrh lrls lrl lra lrms lrr lrrs | lrh > height lls -> bin' (bin' lls ll la lms lr lrls) lrl lra lrms lrr (bin' lrrs l a ms r rs)
      _ -> bin' lls ll la lms lr (bin' lrs l a ms r rs)
bin ls l a ms r (Bin rh rls rl ra rms rr rrs)
  | rh > height ls + 1 =
    case rls of
      Bin rlh rlls rll rla rlms rlr rlrs | rlh > height rrs -> bin' (bin' ls l a ms r rlls) rll rla rlms rlr (bin' rlrs rl ra rms rr rrs)
      _ -> bin' (bin' ls l a ms r rls) rl ra rms rr rrs
bin ls l a ms r rs = bin' ls l a ms r rs

split :: Ord p => p -> STree p a -> Either (TreeError p a) (STree p a, STree p a)
split p = go
  where
    go Tip = pure (Tip, Tip)
    go (Bin _ ls l a ms r rs)
      | p <= l = do
        (ll, lr) <- go ls
        pure (ll, bin lr l a ms r rs)
      | p >= r = do
        (rl, rr) <- go rs
        pure (bin ls l a ms r rl, rr)
      | otherwise = Left MidSplit

insertWith :: Ord p => (a -> a -> Maybe a) -> p -> a -> p -> STree p a -> Either (TreeError p a) (STree p a)
insertWith f il ia ir t
  | il >= ir = Left $ InvalidBounds il ia ir
  | otherwise = go t
  where
    go Tip = pure $ bin Tip il ia Tip ir Tip
    go (Bin h ls l a ms r rs)
      | ir <= l = flip fmap (go ls) $ \ls' -> bin ls' l a ms r rs
      | il >= r = flip fmap (go rs) $ \rs' -> bin ls l a ms r rs'
      | il == l && ir == r = case f ia a of
        Just a' -> pure $ Bin h ls l a' ms r rs
        Nothing -> Left $ OverlappingBounds ia a il ir
      | il >= l && ir <= r = flip fmap (go ms) $ \ms' -> bin ls l a ms' r rs
      | il <= l && ir >= r = do
        (ll, lr) <- split il ls
        (rl, rr) <- split ir rs
        pure $ bin ll il ia (bin lr l a ms r rl) ir rr
      | otherwise = Left $ LexicalError il ia ir t

insert :: Ord p => p -> a -> p -> STree p a -> Either (TreeError p a) (STree p a)
insert il ia ir t
  | il >= ir = Left $ InvalidBounds il ia ir
  | otherwise = go t
  where
    go Tip = pure $ bin Tip il ia Tip ir Tip
    go (Bin _ ls l a ms r rs)
      | ir <= l = flip fmap (go ls) $ \ls' -> bin ls' l a ms r rs
      | il >= r = flip fmap (go rs) $ \rs' -> bin ls l a ms r rs'
      | il == l && ir == r = Left $ OverlappingBounds ia a il ir
      | il >= l && ir <= r = flip fmap (go ms) $ \ms' -> bin ls l a ms' r rs
      | il <= l && ir >= r = do
        (ll, lr) <- split il ls
        (rl, rr) <- split ir rs
        pure $ bin ll il ia (bin lr l a ms r rl) ir rr
      | otherwise = Left $ LexicalError il ia ir t

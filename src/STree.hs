{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module STree where

import Control.Applicative
import Control.Monad
import Data.Maybe (isJust)

data STree p a
  = Tip
  | Bin {-# UNPACK #-} !Int !(STree p a) !p a !(STree p a) !p !(STree p a)
  deriving (Show, Functor, Foldable, Traversable)

instance (Eq p, Eq a) => Eq (STree p a) where
  ta == tb = sTreeList ta == sTreeList tb

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
    f ls l a m r rs
      | p >= l && p < r = Just a
      | p < l = ls
      | p >= r = rs
      | otherwise = error "impossible"

lookupStack :: Ord p => p -> STree p a -> [a]
lookupStack p = foldSTree [] f
  where
    f ls l a m r rs
      | p >= l && p < r = a : m
      | p < l = ls
      | p >= r = rs
      | otherwise = error "impossible"

sTreeList :: STree p a -> [(p, a, p)]
sTreeList t = foldSTree id f t []
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

foldSTreeM :: Monad m => m r -> (r -> p -> a -> r -> p -> r -> m r) -> STree p a -> m r
foldSTreeM fTip fBin = go
  where
    go Tip = fTip
    go (Bin _ mls l a mms r mrs) = do
      ls <- go mls
      ms <- go mms
      rs <- go mrs
      fBin ls l a ms r rs

range :: STree p a -> Maybe (p, p)
range = foldSTree Nothing $ \l pl _ _ pr r -> Just (maybe pl fst l, maybe pr snd r)

check :: Ord p => STree p a -> Bool
check t = checkBounds t && checkHeight t && checkBalance t

checkBounds :: Ord p => STree p a -> Bool
checkBounds t = isJust $ foldSTreeM (pure Nothing) f t
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

checkHeight :: STree p a -> Bool
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

checkBalance :: STree p a -> Bool
checkBalance Tip = True
checkBalance (Bin _ l _ _ m _ r) =
  and
    [ checkBalance l,
      checkBalance r,
      checkBalance m,
      abs (height l - height r) < 2
    ]

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

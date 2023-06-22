{-# LANGUAGE DeriveTraversable #-}

-- | A 'LexTree' is a map designed to reconstruct the lexical structure (tree of scopes) of a source file, given an unordered list of scopes.
-- Values are inserted with a pair source locations as its key.
-- For a given key, we can then ask what the smallest enclosing scope is.
--
-- For example, in the snippet below the smallest scope containing @x@ is @b@.
-- @
--      x
-- |      a      |
--    |  b  |
--                   |   c   |
-- @
--
-- Scopes are not allowed to overlap.
--
-- The purpose of this data structure is to find out what surrounding definition a certain use site belongs to.
module Calligraphy.Util.LexTree
  ( LexTree (..),
    TreeError (..),
    Calligraphy.Util.LexTree.lookup,
    lookupOuter,
    insert,
    emptyLexTree,
    foldLexTree,
    toForest,
    insertWith,
    height,
    toList,
    bin,
    shift,
  )
where

import Control.Applicative
import Data.Tree (Forest)
import qualified Data.Tree as Tree

data LexTree p a
  = Tip
  | Bin
      {-# UNPACK #-} !Int
      -- ^ Height
      !(LexTree p a)
      -- ^ Scopes at the same level, left of this one
      !p
      -- ^ Left-hand bound of this scope (inclusive)
      a
      !(LexTree p a)
      -- ^ Children
      !p
      -- ^ Right-hand bound of this scope (exclusive)
      !(LexTree p a)
      -- ^ Scopes at the same level, right of this entry
  deriving (Show, Functor, Foldable, Traversable)

instance (Eq p, Eq a) => Eq (LexTree p a) where
  ta == tb = toList ta == toList tb

lookup :: Ord p => p -> LexTree p a -> Maybe a
lookup p = foldLexTree Nothing f
  where
    f ls l a m r rs
      | p >= l && p < r = m <|> Just a
      | p < l = ls
      | p >= r = rs
      | otherwise = error "impossible"

lookupOuter :: Ord p => p -> LexTree p a -> Maybe a
lookupOuter p = foldLexTree Nothing f
  where
    f ls l a _ r rs
      | p >= l && p < r = Just a
      | p < l = ls
      | p >= r = rs
      | otherwise = error "impossible"

toList :: LexTree p a -> [(p, a, p)]
toList t = foldLexTree id f t []
  where
    f ls l a m r rs = ls . ((l, a, r) :) . m . rs

foldLexTree :: r -> (r -> p -> a -> r -> p -> r -> r) -> LexTree p a -> r
foldLexTree fTip fBin = go
  where
    go Tip = fTip
    go (Bin _ ls l a ms r rs) = fBin (go ls) l a (go ms) r (go rs)

emptyLexTree :: LexTree p a
emptyLexTree = Tip

toForest :: LexTree p a -> Forest (p, a, p)
toForest lt = foldLexTree id f lt []
  where
    f ls l a m r rs = ls . (Tree.Node (l, a, r) (m []) :) . rs

{-# INLINE height #-}
height :: LexTree p a -> Int
height Tip = 0
height (Bin h _ _ _ _ _ _) = h

shift :: Num p => p -> LexTree p a -> LexTree p a
shift p = go
  where
    go Tip = Tip
    go (Bin h ls l a ms r rs) = Bin h (go ls) (l + p) a (go ms) (r + p) (go rs)

data TreeError p a
  = -- | Nonsensical bounds, i.e. a left-hand bound larger than the right-hand obund
    InvalidBounds p a p
  | -- | Two identical scopes
    OverlappingBounds a a p p
  | -- | An attempt to split halfway through a scope, usually the result of two partially overlapping scopes
    MidSplit
  | -- | Attempting to insert a scope that would not form a tree structure
    LexicalError p a p (LexTree p a)
  deriving (Functor, Foldable, Traversable, Eq, Show)

{-# INLINE bin' #-}
bin' :: LexTree p a -> p -> a -> LexTree p a -> p -> LexTree p a -> LexTree p a
bin' ls l a ms r rs = Bin (max (height ls) (height rs) + 1) ls l a ms r rs

-- | Only works if the height difference of the two trees is at most 2
{-# INLINE bin #-}
bin :: LexTree p a -> p -> a -> LexTree p a -> p -> LexTree p a -> LexTree p a
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

split :: Ord p => p -> LexTree p a -> Either (TreeError p a) (LexTree p a, LexTree p a)
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

{-# INLINE insertWith #-}
insertWith :: Ord p => (a -> a -> Maybe a) -> p -> a -> p -> LexTree p a -> Either (TreeError p a) (LexTree p a)
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

insert :: Ord p => p -> a -> p -> LexTree p a -> Either (TreeError p a) (LexTree p a)
insert = insertWith (\_ _ -> Nothing)

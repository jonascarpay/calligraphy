{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Calligraphy.Util.Lens
  ( Traversal,
    Traversal',
    over,
    forT_,
  )
where

import Data.Functor.Identity

type Traversal s t a b = forall m. Applicative m => (a -> m b) -> (s -> m t)

type Traversal' s a = Traversal s s a a

newtype ConstT m a = ConstT {unConstT :: m ()}
  deriving (Functor)

instance Applicative m => Applicative (ConstT m) where
  {-# INLINE pure #-}
  pure _ = ConstT (pure ())
  {-# INLINE (<*>) #-}
  ConstT mf <*> ConstT ma = ConstT (mf *> ma)

{-# INLINE over #-}
over :: Traversal s t a b -> (a -> b) -> (s -> t)
over t f = runIdentity . t (Identity . f)

{-# INLINE mapT_ #-}
mapT_ :: Applicative m => Traversal s t a b -> (a -> m ()) -> s -> m ()
mapT_ t f = unConstT . t (ConstT . f)

{-# INLINE forT_ #-}
forT_ :: Applicative m => Traversal s t a b -> s -> (a -> m ()) -> m ()
forT_ t = flip (mapT_ t)

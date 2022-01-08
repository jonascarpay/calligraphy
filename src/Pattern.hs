{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Pattern where

import Control.Applicative
import Control.Arrow qualified as Cat
import Control.Category qualified as Cat
import Control.Monad.Except
import Data.Void

-- | CPS'd @ReaderT i (Either e o)@
newtype Pattern e i o = Pattern {runPattern :: forall r. (e -> r) -> (o -> r) -> (i -> r)}

unPattern :: (e -> r) -> (o -> r) -> i -> Pattern e i o -> r
unPattern ng ok i (Pattern p) = p ng ok i

runTotalPattern :: Pattern Void i o -> i -> o
runTotalPattern p = runPattern p absurd id

patternM :: MonadError e m => Pattern e i o -> i -> m o
patternM (Pattern p) = p throwError pure

fromMaybe :: (i -> Maybe o) -> Pattern () i o
fromMaybe f = Pattern $ \ng ok -> maybe (ng ()) ok . f

fromMaybe2 :: (i1 -> i2 -> Maybe o) -> Pattern () (i1, i2) o
fromMaybe2 = fromMaybe . uncurry

fromEither :: (i -> Either e o) -> Pattern e i o
fromEither f = Pattern $ \ng ok -> either ng ok . f

mapError :: (e' -> e) -> Pattern e' i o -> Pattern e i o
mapError f (Pattern p) = Pattern $ \ng -> p (ng . f)

setError :: e -> Pattern e' i o -> Pattern e i o
setError err (Pattern p) = Pattern $ \ng -> p (const $ ng err)

throws :: (i -> e) -> Pattern e i o
throws f = Pattern $ \ng _ i -> ng (f i)

infixl 3 |>

infixl 3 <|

(|>) :: Pattern el i o -> Pattern er i o -> Pattern er i o
Pattern l |> Pattern r = Pattern $ \ng ok i ->
  l (const $ r ng ok i) ok i

(<|) :: Pattern el i o -> Pattern er i o -> Pattern el i o
Pattern l <| Pattern r = Pattern $ \ng ok i ->
  l (\e -> r (const $ ng e) ok i) ok i

instance MonadError e (Pattern e i) where
  throwError e = Pattern $ \ng _ _ -> ng e
  catchError (Pattern pt) fc = Pattern $ \ng ok i ->
    pt (\e -> runPattern (fc e) ng ok i) ok i

instance Functor (Pattern e i) where
  fmap f (Pattern p) = Pattern $ \n y -> p n (y . f)

instance Applicative (Pattern e i) where
  pure a = Pattern $ \_ ok _ -> ok a
  Pattern pab <*> Pattern pa = Pattern $ \ng ok i ->
    pab ng (\fab -> pa ng (ok . fab) i) i

instance Monoid e => Alternative (Pattern e i) where
  empty = Pattern $ \ng _ _ -> ng mempty
  Pattern l <|> Pattern r = Pattern $ \ng ok i ->
    l (\e' -> r (ng . mappend e') ok i) ok i

instance Monad (Pattern e i) where
  Pattern pa >>= apb = Pattern $ \ng ok i ->
    pa ng (\a -> runPattern (apb a) ng ok i) i

instance Cat.Category (Pattern e) where
  id = Pattern $ \_ x -> x
  Pattern g . Pattern f = Pattern $ \ng ok -> f ng (g ng ok)

instance Semigroup e => Cat.Arrow (Pattern e) where
  arr f = Pattern $ \_ ok i -> ok (f i)
  Pattern pl *** Pattern pr = Pattern $ \ng ok (il, ir) ->
    pl
      ( \el ->
          pr
            (\er -> ng (el <> er))
            (\_ -> ng el)
            ir
      )
      ( \ol ->
          pr
            (\er -> ng er)
            (\or -> ok (ol, or))
            ir
      )
      il

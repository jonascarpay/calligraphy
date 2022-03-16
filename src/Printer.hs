{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Printer where

import Control.Monad.RWS
import Control.Monad.State
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB

newtype Printer a = Printer {unPrinter :: RWS Int () Builder a}
  deriving (Functor, Applicative, Monad)
  deriving (Semigroup, Monoid) via (Ap Printer a)

type Prints a = a -> Printer ()

runPrinter :: Printer () -> Text
runPrinter (Printer p) = TL.toStrict . TB.toLazyText . fst $ execRWS p 0 mempty

class Monad m => MonadPrint m where
  line :: Builder -> m ()
  indent :: m a -> m a

instance MonadPrint Printer where
  {-# INLINE indent #-}
  indent (Printer p) = Printer $ local (+ 4) p

  {-# INLINE line #-}
  line t = Printer $ do
    n <- ask
    modify $
      flip mappend $ fold (replicate n (TB.singleton ' ')) <> t <> TB.singleton '\n'

instance MonadPrint m => MonadPrint (StateT s m) where
  line = lift . line
  indent (StateT m) = StateT $ indent . m

{-# INLINE brack #-}
brack :: MonadPrint m => String -> String -> m a -> m a
brack pre post inner = strLn pre *> indent inner <* strLn post

{-# INLINE strLn #-}
strLn :: MonadPrint m => String -> m ()
strLn = line . TB.fromString

textLn :: MonadPrint m => Text -> m ()
textLn = line . TB.fromText

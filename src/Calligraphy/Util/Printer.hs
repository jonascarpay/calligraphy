{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Calligraphy.Util.Printer
  ( Printer,
    Prints,
    runPrinter,
    indent,
    brack,
    strLn,
    textLn,
    showLn,
  )
where

import Control.Monad.RWS
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import Data.Monoid (Ap(..))

-- | An monadic interface to a fairly primitive line printer.
-- It maintains an indentation level, and provides efficient concatenation through 'Builder', and that's it.
newtype Printer a = Printer {_unPrinter :: RWS Int () Builder a}
  deriving newtype (Functor, Applicative, Monad)
  deriving (Semigroup, Monoid) via (Ap Printer a)

type Prints a = a -> Printer ()

runPrinter :: Printer () -> Text
runPrinter (Printer p) = TL.toStrict . TB.toLazyText . fst $ execRWS p 0 mempty

{-# INLINE indent #-}
indent :: Printer a -> Printer a
indent (Printer p) = Printer $ local (+ 4) p

{-# INLINE line #-}
line :: Builder -> Printer ()
line t = Printer $ do
  n <- ask
  modify $
    flip mappend $
      fold (replicate n (TB.singleton ' ')) <> t <> TB.singleton '\n'

{-# INLINE brack #-}
brack :: String -> String -> Printer a -> Printer a
brack pre post inner = strLn pre *> indent inner <* strLn post

{-# INLINE strLn #-}
strLn :: String -> Printer ()
strLn = line . TB.fromString

{-# INLINE textLn #-}
textLn :: Text -> Printer ()
textLn = line . TB.fromText

{-# INLINE showLn #-}
showLn :: (Show a) => a -> Printer ()
showLn = strLn . show

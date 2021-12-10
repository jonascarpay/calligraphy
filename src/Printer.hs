module Printer where

import Control.Monad.RWS
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB

type Printer = RWS Int () Builder

type Prints a = a -> Printer ()

runPrinter :: Printer () -> Text
runPrinter p = TL.toStrict . TB.toLazyText . fst $ execRWS p 0 mempty

indent :: Printer a -> Printer a
indent = local (+ 4)

line :: Prints Builder
line t = do
  n <- ask
  modify $
    flip mappend $ fold (replicate n (TB.singleton ' ')) <> t <> TB.singleton '\n'

strLn :: Prints String
strLn = line . TB.fromString

textLn :: Prints Text
textLn = line . TB.fromText

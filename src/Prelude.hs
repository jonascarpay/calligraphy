module Prelude 
  ( module P
  , module Control.Monad
  , module Data.Either
  , module Data.Foldable
  , module Data.List
  , module Data.Monoid
  ) where

import Control.Monad
import Data.Either
import Data.Foldable hiding (toList)
import Data.List (last, (++), replicate, zip, filter, isPrefixOf)
import Data.Monoid (Monoid, mempty, mconcat, mappend, Ap(..))
import qualified BasePrelude as P 
import BasePrelude

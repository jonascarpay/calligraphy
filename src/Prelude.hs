{-# LANGUAGE CPP #-}
module Prelude 
  ( module P
  , module Control.Monad
  , module Data.Either
  , module Data.Foldable
  , module Data.List
  , module Data.Monoid
#if MIN_VERSION_ghc(9,0,0)
  , module GHC.Iface.Ext.Binary
  , module GHC.Iface.Ext.Types
  , module GHC.Types.Name.Cache
  , module GHC.Types.SrcLoc
  , module GHC.Utils.Outputable 
#else
  , module HieBin
  , module HieTypes
  , module NameCache
  , module SrcLoc
#endif
  ) where

import Control.Monad
import Data.Either
import Data.Foldable hiding (toList)
import Data.List (last, (++), replicate, zip, filter, isPrefixOf)
import Data.Monoid (Monoid, mempty, mconcat, mappend, Ap(..))
import qualified BasePrelude as P 
import BasePrelude
#if MIN_VERSION_ghc(9,0,0)
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Types.Name.Cache
import GHC.Types.SrcLoc
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
#else
import HieBin
import HieTypes
import NameCache
import SrcLoc
#endif

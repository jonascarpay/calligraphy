{-# LANGUAGE NamedFieldPuns #-}

module Calligraphy.Phases.EdgeFilter (EdgeFilterConfig, filterEdges, pEdgeFilterConfig) where

import Calligraphy.Util.Types
import qualified Data.Set as Set
import Options.Applicative

data EdgeFilterConfig = EdgeFilterConfig
  { explicit :: Bool,
    types :: Bool,
    hideOverlapping :: Bool,
    hideLoops :: Bool
  }

filterEdges :: EdgeFilterConfig -> Modules -> Modules
filterEdges
  EdgeFilterConfig {explicit, types, hideOverlapping, hideLoops}
  (Modules mods calls typeEdges) =
    Modules mods calls' types'
    where
      filterRecursive = if hideLoops then Set.filter (uncurry (/=)) else id
      filterOverlap = if hideOverlapping then (Set.\\ calls') else id
      calls' = filterRecursive $ if explicit then calls else mempty
      types' = filterOverlap . filterRecursive $ if types then typeEdges else mempty

pEdgeFilterConfig :: Parser EdgeFilterConfig
pEdgeFilterConfig =
  EdgeFilterConfig
    <$> flag True False (long "no-value-deps" <> help "Ignore value dependencies. This means they won't be drawn or included in dependency calculations.")
    <*> flag True False (long "no-type-deps" <> help "Ignore type dependencies. This means they won't be drawn or included in dependency calculations.")
    <*> flag False True (long "hide-double-edges" <> help "Hide type edges if value edges already exist.")
    <*> flag False True (long "hide-loops" <> help "Hide loops, i.e. edges that start and end at the same node.")

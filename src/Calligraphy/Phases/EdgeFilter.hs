{-# LANGUAGE NamedFieldPuns #-}

module Calligraphy.Phases.EdgeFilter (EdgeFilterConfig, filterEdges, pEdgeFilterConfig) where

import Calligraphy.Util.Types
import qualified Data.Set as Set
import Options.Applicative

data EdgeFilterConfig = EdgeFilterConfig
  { hideOverlapping :: Bool,
    hideLoops :: Bool
  }

filterEdges :: EdgeFilterConfig -> Modules -> Modules
filterEdges
  EdgeFilterConfig {hideOverlapping, hideLoops}
  (Modules mods calls types) =
    Modules mods (filterRecursive calls) (filterOverlap . filterRecursive $ types)
    where
      filterRecursive = if hideLoops then Set.filter (uncurry (/=)) else id
      filterOverlap = if hideOverlapping then (Set.\\ calls) else id

pEdgeFilterConfig :: Parser EdgeFilterConfig
pEdgeFilterConfig =
  EdgeFilterConfig
    <$> flag False True (long "hide-double-edges" <> help "Hide type edges if value edges already exist.")
    <*> flag False True (long "hide-loops" <> help "Hide loops, i.e. edges that start and end at the same node.")

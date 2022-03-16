{-# LANGUAGE NamedFieldPuns #-}

module EdgeFilter (EdgeFilterConfig, filterEdges, pEdgeFilterConfig) where

import qualified Data.Set as Set
import Options.Applicative
import Parse

data EdgeFilterConfig = EdgeFilterConfig
  { explicit :: Bool,
    inferred :: Bool,
    overlapping :: Bool,
    hideLoops :: Bool
  }

filterEdges :: EdgeFilterConfig -> Modules -> Modules
filterEdges
  EdgeFilterConfig {explicit, inferred, overlapping, hideLoops}
  (Modules mods calls infers) =
    Modules mods calls' infers'
    where
      filterRecursive = if hideLoops then Set.filter (uncurry (/=)) else id
      filterOverlap = if overlapping then (Set.\\ calls') else id
      calls' = filterRecursive $ if explicit then calls else mempty
      infers' = filterOverlap . filterRecursive $ if inferred then infers else mempty

pEdgeFilterConfig :: Parser EdgeFilterConfig
pEdgeFilterConfig =
  EdgeFilterConfig
    <$> flag True False (long "no-explicit" <> help "Don't use explitic dependencies. Used with --infer to only use type-inferred dependencies.")
    <*> flag False True (long "inferred" <> help "Use type-inferred dependencies.")
    <*> flag False True (long "no-hide-overlaps" <> help "Don't hide an inferred dependencies if an explicit dependency between the nodes already exists.")
    <*> flag False True (long "no-loops" <> help "Hide loops, i.e. edges that start and end at the same node.")

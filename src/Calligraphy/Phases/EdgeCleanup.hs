{-# LANGUAGE NamedFieldPuns #-}

-- | This modules collects some opinionated common-sense heuristics for removing edges that are probably redundant.
module Calligraphy.Phases.EdgeCleanup (EdgeCleanupConfig, cleanupEdges, pEdgeCleanupConfig) where

import Calligraphy.Util.Types
import Control.Monad.State.Strict
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree
import Options.Applicative

data EdgeCleanupConfig = EdgeCleanupConfig
  { cleanDoubles :: Bool,
    cleanLoops :: Bool,
    cleanData :: Bool,
    cleanClass :: Bool
  }

cleanupEdges :: EdgeCleanupConfig -> CallGraph -> CallGraph
cleanupEdges
  EdgeCleanupConfig {cleanDoubles, cleanLoops, cleanData, cleanClass}
  (CallGraph mods calls types) =
    CallGraph mods (cleanLoopsFn calls) (cleanLoopsFn . cleanDoublesFn . cleanDataFn . cleanClassFn $ types)
    where
      cleanLoopsFn = if cleanLoops then Set.filter (uncurry (/=)) else id
      cleanDoublesFn = if cleanDoubles then (Set.\\ calls) else id
      cleanDataFn = if cleanData then (Set.\\ dataEdges) else id
      cleanClassFn = if cleanClass then (Set.\\ classEdges) else id
      dataEdges = execState (forT_ (traverse . modForest . traverse) mods go) mempty
        where
          go :: Tree Decl -> State (Set (Key, Key)) ()
          go (Node (Decl _ k _ _ DataDecl _) children) = void $ (traverse . traverse) (\d -> modify (Set.insert (declKey d, k))) children
          go (Node _ children) = mapM_ go children
      classEdges = execState (forT_ (traverse . modForest . traverse) mods go) mempty
        where
          go :: Tree Decl -> State (Set (Key, Key)) ()
          go (Node (Decl _ k _ _ ClassDecl _) children) = void $ (traverse . traverse) (\d -> modify (Set.insert (declKey d, k))) children
          go (Node _ children) = mapM_ go children

pEdgeCleanupConfig :: Parser EdgeCleanupConfig
pEdgeCleanupConfig =
  EdgeCleanupConfig
    <$> flag True False (long "no-clean-double-edges" <> help "Don't remove type edges when value edges already exist.")
    <*> flag True False (long "no-clean-loops" <> help "Don't remove edges that start and stop at the same node, i.e. simple recursion.")
    <*> flag True False (long "no-clean-data" <> help "Don't remove type edges constructors/records back to the parent data type. These are removed by default because their behavior is unreliable, and they're generally redundant.")
    <*> flag True False (long "no-clean-classes" <> help "Don't remove type edges from class members back to the parent class. These are removed by default because their behavior is unreliable, and they're generally redundant.")

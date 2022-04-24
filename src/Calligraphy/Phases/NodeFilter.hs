{-# LANGUAGE RecordWildCards #-}

module Calligraphy.Phases.NodeFilter (filterNodes, NodeFilterConfig, pNodeFilterConfig) where

import Calligraphy.Util.Types
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.Tree
import Options.Applicative
import Prelude hiding (filter)

data NodeFilterConfig = NodeFilterConfig
  { hideLocals :: Bool,
    hideValues :: Bool,
    hideRecords :: Bool,
    hideConstructors :: Bool,
    hideData :: Bool,
    hideClasses :: Bool
  }

-- | If p does not hold, neither that node nor its children are included.
-- Compare this to 'pruneModules', where a node is included if p holds for it or any of its children.
filterModules :: (Decl -> Bool) -> Modules -> Modules
filterModules p (Modules modules calls types) = removeDeadCalls $ Modules modules' calls types
  where
    modules' = over (traverse . modForest) filterForest modules
    filterForest :: Forest Decl -> Forest Decl
    filterForest = mapMaybe filterTree
    filterTree :: Tree Decl -> Maybe (Tree Decl)
    filterTree (Node decl children)
      | p decl = Just (Node decl (filterForest children))
      | otherwise = Nothing

nodeFilter :: NodeFilterConfig -> Decl -> Bool
nodeFilter NodeFilterConfig {..} (Decl _ _ isExp typ _) = expOk && typOk
  where
    expOk = isExp || not hideLocals
    typOk = not $ case typ of
      ValueDecl -> hideValues
      RecDecl -> hideRecords
      ConDecl -> hideConstructors
      DataDecl -> hideData
      ClassDecl -> hideClasses

filterNodes :: NodeFilterConfig -> Modules -> Modules
filterNodes cfg = filterModules (nodeFilter cfg)

pNodeFilterConfig :: Parser NodeFilterConfig
pNodeFilterConfig =
  NodeFilterConfig
    <$> switch (long "exports-only" <> long "hide-local-bindings" <> help "Don't draw non-exported bindings.")
    <*> switch (long "hide-values" <> help "Hide nodes for values")
    <*> switch (long "hide-records" <> help "Hide nodes for record fields")
    <*> switch (long "hide-constructors" <> help "Hide nodes for data constructors")
    <*> switch (long "hide-data" <> help "Hide nodes for types and type construcstors")
    <*> switch (long "hide-classes" <> help "Hide nodes for type classes")

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Calligraphy.Phases.NodeFilter (filterNodes, NodeFilterConfig, pNodeFilterConfig) where

import Calligraphy.Phases.Parse
import Control.Monad.State
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as EnumSet
import Data.Maybe (catMaybes)
import Data.Monoid
import qualified Data.Set as Set
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

filterModules :: (Decl -> Bool) -> Modules -> Modules
filterModules p (Modules modules calls infers) = Modules modules' calls' infers'
  where
    (modules', outputKeys) = runState ((traverse . traverse) filterForest modules) mempty
    filterForest :: Forest Decl -> State (EnumSet Key) (Forest Decl)
    filterForest = fmap catMaybes . mapM filterTree
    filterTree :: Tree Decl -> State (EnumSet Key) (Maybe (Tree Decl))
    filterTree (Node decl children)
      | p decl = do
        children' <- filterForest children
        Just (Node decl children') <$ modify (EnumSet.insert (declKey decl))
      | otherwise = pure Nothing
    -- TODO combine with pruneModules
    calls' = Set.filter (\(a, b) -> EnumSet.member a outputKeys && EnumSet.member b outputKeys) calls
    infers' = Set.filter (\(a, b) -> EnumSet.member a outputKeys && EnumSet.member b outputKeys) infers

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
    <$> switch (long "hide-local-bindings" <> long "exports-only" <> help "Don't draw non-exported bindings.")
    <*> switch (long "hide-values" <> help "Hide nodes for values")
    <*> switch (long "hide-records" <> help "Hide nodes for record fields")
    <*> switch (long "hide-constructors" <> help "Hide nodes for data constructors")
    <*> switch (long "hide-data" <> help "Hide nodes for types and type construcstors")
    <*> switch (long "hide-classes" <> help "Hide nodes for type classes")

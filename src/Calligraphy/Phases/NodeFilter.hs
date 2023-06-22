{-# LANGUAGE RecordWildCards #-}

-- | This modules manages the two ways we remove nodes from a graph; collapsing and hiding.
--
-- Collapsing means absorbing a node's descendants into itself, including all edges.
--
-- Hiding means removing a node (and its descendants), moving the edges to the node's parent, if a parent exist.
--
-- There's also the special option --collapse-modules.
-- It's undeniably a little hacky, but for now this is the best home for that functionality.
-- Functionality-wise, it's still essentially just collapsing nodes into one another.
-- The thing that makes it hacky is that it then uses a value node to represent a module.
-- This is not actually a huge deal, because no other module actually cares about the node type, but it's something to watch out for.
-- There's more design discussion on https://github.com/jonascarpay/calligraphy/pull/5
module Calligraphy.Phases.NodeFilter
  ( filterNodes
  , NodeFilterConfig (..)
  , pNodeFilterConfig
  ) where

import Prelude hiding (Decl)

import Control.Monad.State
import Data.EnumMap (EnumMap)
import Data.Maybe (catMaybes)
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Options.Applicative
import qualified Data.EnumMap as EnumMap

import Calligraphy.Util.Types

data Mode = Show | Collapse | Hide
  deriving (Eq, Show)

data NodeFilterConfig = NodeFilterConfig
  { hideLocals :: Bool,
    collapseModules :: Bool,
    collapseClasses :: Mode,
    collapseData :: Mode,
    collapseValues :: Mode,
    collapseConstructors :: Mode,
    hideRecords :: Bool
  }

pNodeFilterConfig :: Parser NodeFilterConfig
pNodeFilterConfig =
  NodeFilterConfig
    <$> switch (long "exports-only" <> long "hide-local-bindings" <> help "Remove all non-exported bindings, merging all edges into its parent, if one exist.")
    <*> switch (long "collapse-modules" <> help "Collapse all nodes into a single node per module.")
    <*> pMode "classes" "class"
    <*> pMode "data" "data"
    <*> pMode "values" "value"
    <*> pMode "constructors" "constructor"
    <*> flag False True (long "hide-records" <> help "Remove all record nodes.")
  where
    pMode :: String -> String -> Parser Mode
    pMode flagName helpName =
      flag' Collapse (long ("collapse-" <> flagName) <> help collapseHelp)
        <|> flag' Hide (long ("hide-" <> flagName) <> help hideHelp)
        <|> pure Show
      where
        collapseHelp = "Remove all " <> helpName <> " nodes's children, merging the children's edges into itself."
        hideHelp = "Remove all " <> helpName <> " nodes and their children."

filterNodes :: NodeFilterConfig -> CallGraph -> CallGraph
filterNodes NodeFilterConfig {..} (CallGraph modules calls types) =
  let (modules', reps) =
        flip runState mempty $
          forM modules $
            if collapseModules
              then collapseModule
              else modForest (fmap catMaybes . traverse (go Nothing))
   in CallGraph modules' (rekeyCalls reps calls) (rekeyCalls reps types)
  where
    collapseModule :: Module -> State (EnumMap Key Key) Module
    collapseModule (Module modname path []) = pure $ Module modname path []
    collapseModule (Module modname path forest@(Tree.Node rep _ : _)) = do
      let repKey = declKey rep
      forT_ forestT forest $ \decl -> assoc (declKey decl) repKey
      pure $ Module modname path [Tree.Node (Decl modname repKey mempty True ValueDecl (Loc 1 1)) []]

    shouldCollapse :: Decl -> Bool
    shouldCollapse decl = case declType decl of
      ValueDecl -> collapseValues == Collapse
      ClassDecl -> collapseClasses == Collapse
      ConDecl -> collapseConstructors == Collapse
      DataDecl -> collapseData == Collapse
      _ -> False

    shouldHide :: Decl -> Bool
    shouldHide decl = typ (declType decl) || (hideLocals && not (declExported decl))
      where
        typ ClassDecl = collapseClasses == Hide
        typ DataDecl = collapseData == Hide
        typ ValueDecl = collapseValues == Hide
        typ ConDecl = collapseConstructors == Hide
        typ RecDecl = hideRecords

    go :: Maybe Decl -> Tree Decl -> State (EnumMap Key Key) (Maybe (Tree Decl))
    go mparent node@(Tree.Node decl children)
      | shouldHide decl = do
          forM_ mparent $ \parent ->
            forM_ node $ \child -> assoc (declKey child) (declKey parent)
          pure Nothing
      | shouldCollapse decl = do
          forM_ node $ \child ->
            assoc (declKey child) (declKey decl)
          pure $ Just $ Tree.Node decl []
      | otherwise = do
          assoc (declKey decl) (declKey decl)
          children' <- catMaybes <$> mapM (go (Just decl)) children
          pure $ Just $ Tree.Node decl children'

    assoc :: Key -> Key -> State (EnumMap Key Key) ()
    assoc key rep = modify (EnumMap.insert key rep)

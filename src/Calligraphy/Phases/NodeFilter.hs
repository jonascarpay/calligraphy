{-# LANGUAGE RecordWildCards #-}

-- | This modules manages the two ways we remove nodes from a graph; collapsing and hiding.
--
-- Collapsing means absorbing a node's descendants into itself, including all edges.
--
-- Hiding means removing a node (and its descendants), moving the edges to the node's parent, if a parent exist.
module Calligraphy.Phases.NodeFilter (filterNodes, NodeFilterConfig, pNodeFilterConfig) where

import Calligraphy.Util.Types
import Control.Monad.State
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.Maybe (catMaybes)
import Data.Tree
import Options.Applicative

data Mode = Show | Collapse | Hide
  deriving (Eq, Show)

data NodeFilterConfig = NodeFilterConfig
  { hideLocals :: Bool,
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
  let (modules', reps) = flip runState mempty $ (traverse . modForest) (fmap catMaybes . traverse (go Nothing)) modules
   in CallGraph modules' (rekeyCalls reps calls) (rekeyCalls reps types)
  where
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
    go mparent node@(Node decl children)
      | shouldHide decl = do
          forM_ mparent $ \parent ->
            forM_ node $ \child -> assoc (declKey child) (declKey parent)
          pure Nothing
      | shouldCollapse decl = do
          forM_ node $ \child ->
            assoc (declKey child) (declKey decl)
          pure $ Just $ Node decl []
      | otherwise = do
          assoc (declKey decl) (declKey decl)
          children' <- catMaybes <$> mapM (go (Just decl)) children
          pure $ Just $ Node decl children'

    assoc :: Key -> Key -> State (EnumMap Key Key) ()
    assoc key rep = modify (EnumMap.insert key rep)

{-# LANGUAGE RecordWildCards #-}

-- | This modules manages the two ways we remove nodes from a graph; collapsing and hiding.
--
-- Collapsing means folding a node's descendants into itself, merging all incoming and outcoming edges.
--
-- Hiding means removing a node (and its descendants), moving the edges into the node's parent, if a parent exist.
--
-- Since these are essentially the same thing from different perspectives, they are handled by the same module.
module Calligraphy.Phases.Collapse (collapse, CollapseConfig, pCollapseConfig) where

import Calligraphy.Util.Types
import Control.Monad.State
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.Maybe (catMaybes)
import Data.Tree
import Options.Applicative

data Mode = Show | Collapse | Hide
  deriving (Eq, Show)

data CollapseConfig = CollapseConfig
  { hideLocals :: Bool,
    collapseClasses :: Mode,
    collapseData :: Mode,
    collapseValues :: Mode,
    collapseConstructors :: Mode,
    hideRecords :: Bool
  }

pCollapseConfig :: Parser CollapseConfig
pCollapseConfig =
  CollapseConfig
    <$> switch (long "exports-only" <> long "hide-local-bindings" <> help "Remove all non-exported bindings, merging all edges into its parent, if one exist.")
    <*> pMode "classes" "Remove all class nodes's children, merging the children's edges into itself." "Remove all class nodes and their children."
    <*> pMode "data" "Remove all data nodes's children, merging the children's edges into itself." "Remove all data nodes and their children, merging all edges into the data node's parent, if one exists."
    <*> pMode "values" "Remove all value nodes's children, merging the children's edges into itself." "Remove all value nodes and their children, merging all edges into the value node's parent, if one exists."
    <*> pMode "constructors" "Remove all constructor nodes's children, merging the children's edges into itself." "Remove all constructor nodes and their children, merging all edges into the constructor node's parent, if one exists."
    <*> flag False True (long "hide-records" <> help "Remove all record nodes.")
  where
    pMode :: String -> String -> String -> Parser Mode
    pMode flagName collapseHelp hideHelp =
      flag' Collapse (long ("collapse-" <> flagName) <> help collapseHelp)
        <|> flag' Hide (long ("hide-" <> flagName) <> help hideHelp)
        <|> pure Show

collapse :: CollapseConfig -> CallGraph -> CallGraph
collapse CollapseConfig {..} (CallGraph modules calls types) =
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

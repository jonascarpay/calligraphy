{-# LANGUAGE RecordWildCards #-}

-- | Prepare the call graph for rendering
module Calligraphy.Phases.Render.Common
  ( RenderConfig (..),
    pRenderConfig,
    renderGraph,
    RenderError,
    ppRenderError,
    ID,
    RenderGraph (..),
    RenderModule (..),
    RenderNode (..),
    ClusterModules (..),
    if',
  )
where

import Prelude hiding (Decl, DeclType)
import Calligraphy.Util.Printer (Prints, strLn)
import Calligraphy.Util.Types (CallGraph (..), Decl (..), DeclType, GHCKey (unGHCKey), Key (..), Loc (..), Module (..))
import Control.Applicative ((<|>))
import Data.Bifunctor (bimap)
import qualified Data.EnumSet as EnumSet
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree)
import Options.Applicative (Parser, flag, flag', help, long)

data RenderConfig = RenderConfig
  { showCalls :: Bool,
    showTypes :: Bool,
    showKey :: Bool,
    showGHCKeys :: Bool,
    showModulePath :: Bool,
    locMode :: LocMode,
    clusterModules :: ClusterModules
  }

data ClusterModules
  = ClusterNever
  | ClusterWhenMultiple
  | ClusterAlways

pRenderConfig :: Parser RenderConfig
pRenderConfig =
  RenderConfig
    <$> flag True False (long "hide-calls" <> help "Don't show call arrows")
    <*> flag True False (long "hide-types" <> help "Don't show type arrows")
    <*> flag False True (long "show-key" <> help "Show internal keys with identifiers. Useful for debugging.")
    <*> flag False True (long "show-ghc-key" <> help "Show GHC keys with identifiers. Useful for debugging.")
    <*> flag False True (long "show-module-path" <> help "Show a module's filepath instead of its name")
    <*> pLocMode
    <*> pClusterModules

pClusterModules :: Parser ClusterModules
pClusterModules =
  flag' ClusterNever (long "no-cluster-modules" <> help "Don't draw modules as a cluster.")
    <|> flag' ClusterAlways (long "force-cluster-modules" <> help "Draw modules as a cluster, even if there is only one.")
    <|> pure ClusterWhenMultiple

-- | A directly printable string uniquely identifying a declaration.
type ID = String

-- | A representation of the call graph that's convenient for rendering.
-- Structurally, it's the same as 'CallGraph', in that it's a tree of nodes and a flat list of edges.
-- The differences is that as much of the non-backend-specific preprocessing has already been taken care of.
--   - Nodes and modules have a unique string ID
--   - Nodes and modules contain their desired label
--   - Render roots are guaranteed to be non-empty
--   - Set of calls and types are empty on --hide-{calls, types}
--   - Modules are flattened depending on --no-cluster-modules
data RenderGraph = RenderGraph
  { renderRoots :: Either (NonEmpty RenderModule) (NonEmpty (Tree RenderNode)), -- Right if --no-cluster-modules
    callEdges :: Set (ID, ID), -- empty if --hide-calls
    typeEdges :: Set (ID, ID) -- empty if --hide-types
  }

data RenderModule = RenderModule
  { moduleLabel :: String,
    moduleId :: ID,
    moduleDecls :: NonEmpty (Tree RenderNode)
  }

data RenderNode = RenderNode
  { nodeId :: ID,
    nodeType :: DeclType,
    nodeLabelLines :: [String],
    nodeExported :: Bool
  }

data LocMode = Hide | Line | LineCol

data RenderError = EmptyGraph

ppRenderError :: Prints RenderError
ppRenderError EmptyGraph = strLn "Output graph is empty"

renderGraph :: RenderConfig -> CallGraph -> Either RenderError RenderGraph
renderGraph
  RenderConfig {..}
  (CallGraph modules calls types) =
    case nonEmpty (mapMaybe (uncurry mkModule) (zip modules [0 ..])) of
      Nothing -> Left EmptyGraph
      Just neModules ->
        pure $
          RenderGraph
            ( let cluster = case clusterModules of
                    ClusterAlways -> True
                    ClusterWhenMultiple -> length neModules > 1
                    ClusterNever -> False
               in if cluster then Left neModules else Right (neModules >>= moduleDecls)
            )
            (if showCalls then Set.map (bimap keyId keyId) calls else Set.empty)
            (if showTypes then Set.map (bimap keyId keyId) types else Set.empty)
    where
      keyId :: Key -> ID
      keyId (Key k) = "node_" <> show k

      mkModule :: Module -> Int -> Maybe RenderModule
      mkModule (Module name path decls) ix =
        (\ne -> RenderModule label ("module_" <> show ix) (fmap mkNode <$> ne)) <$> nonEmpty decls
        where
          label
            | showModulePath = path
            | otherwise = name

      mkNode :: Decl -> RenderNode
      mkNode (Decl name key ghcKeys x t loc) = RenderNode (keyId key) t (catMaybes lbls) x
        where
          lbls =
            [ pure name,
              if' showKey $ show (unKey key),
              if' showGHCKeys $ "GHC Keys: " <> (unwords . fmap (show . unGHCKey) . EnumSet.toList $ ghcKeys),
              case locMode of
                Hide -> Nothing
                Line -> Just ('L' : show (locLine loc))
                LineCol -> Just (show loc)
            ]

pLocMode :: Parser LocMode
pLocMode =
  flag' Line (long "show-line" <> help "Show line numbers")
    <|> flag' LineCol (long "show-line-col" <> help "Show line and column numbers")
    <|> pure Hide

-- TODO this needs to be moved to an appropriate Util/Lib module
if' :: Bool -> a -> Maybe a
if' True a = Just a
if' False _ = Nothing

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering takes a callgraph, and produces a dot file
module Calligraphy.Phases.Render.GraphViz
  ( GraphVizConfig,
    pGraphVizConfig,
    renderGraphViz,
  )
where

import Calligraphy.Phases.Render.Common
import Calligraphy.Prelude hiding (DeclType)
import Calligraphy.Util.Printer
import Calligraphy.Util.Types
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Options.Applicative hiding (style)
import Text.Show (showListWith)

data GraphVizConfig = GraphVizConfig
  { showChildArrowhead :: Bool,
    clusterGroups :: Bool,
    splines :: Bool,
    reverseDependencyRank :: Bool
  }

pGraphVizConfig :: Parser GraphVizConfig
pGraphVizConfig =
  GraphVizConfig
    <$> flag False True (long "show-child-arrowhead" <> help "Put an arrowhead at the end of a parent-child edge")
    <*> flag True False (long "no-cluster-trees" <> help "Don't draw definition trees as a cluster.")
    <*> flag True False (long "no-splines" <> help "Render arrows as straight lines instead of splines")
    <*> flag False True (long "reverse-dependency-rank" <> help "Make dependencies have lower rank than the dependee, i.e. show dependencies above their parent.")

renderGraphViz :: GraphVizConfig -> Prints RenderGraph
renderGraphViz GraphVizConfig {..} (RenderGraph roots calls types) = do
  brack "digraph calligraphy {" "}" $ do
    unless splines $ textLn "splines=false;"
    textLn "node [style=filled fillcolor=\"#ffffffcf\"];"
    textLn "graph [outputorder=edgesfirst];"
    case roots of
      Left modules -> mapM_ printModule modules
      Right trees -> mapM_ printTree trees
    forM_ calls $ \(caller, callee) ->
      if reverseDependencyRank
        then edge caller callee []
        else edge callee caller ["dir" .= "back"]
    forM_ types $ \(caller, callee) ->
      if reverseDependencyRank
        then edge caller callee ["style" .= "dotted"]
        else edge callee caller ["style" .= "dotted", "dir" .= "back"]
  where
    printTree :: Prints (Tree RenderNode)
    printTree (Tree.Node nodeInfo children) = wrapCluster $ do
      printNode nodeInfo
      forM_ children $ \child@(Tree.Node childInfo _) -> do
        printTree child
        edge (nodeId nodeInfo) (nodeId childInfo) . catMaybes $
          [ pure ("style" .= "dashed"),
            if' (not showChildArrowhead) ("arrowhead" .= "none")
          ]
      where
        wrapCluster inner
          | clusterGroups && not (null children) = brack ("subgraph cluster_" <> nodeId nodeInfo <> " {") "}" $ do
              textLn "style=invis;"
              inner
          | otherwise = inner

    printModule :: Prints RenderModule
    printModule (RenderModule lbl modId trees) =
      brack ("subgraph cluster_module_" <> modId <> " {") "}" $ do
        strLn $ "label=" <> show lbl <> ";"
        forM_ trees printTree

    printNode :: Prints RenderNode
    printNode (RenderNode nId typ lbll exported) =
      strLn $ nId <> " " <> renderAttrs attrs
      where
        attrs =
          [ "label" .= ("\"" <> intercalate "\n" lbll <> "\""),
            "shape" .= nodeShape typ,
            "style" .= nodeStyle
          ]
        nodeStyle =
          show . intercalate ", " . catMaybes $
            [ if' (typ == RecDecl) "rounded",
              if' (not exported) "dashed",
              pure "filled"
            ]

nodeShape :: DeclType -> String
nodeShape DataDecl = "octagon"
nodeShape ConDecl = "box"
nodeShape RecDecl = "box"
nodeShape ClassDecl = "house"
nodeShape ValueDecl = "ellipse"

edge :: ID -> ID -> Attributes -> Printer ()
edge from to attrs = strLn $ show from <> " -> " <> show to <> " " <> renderAttrs attrs

(.=) :: String -> String -> (String, String)
(.=) = (,)

renderAttrs :: Attributes -> String
renderAttrs attrs = showListWith (\(key, val) -> showString key . showChar '=' . showString val) attrs ";"

type Attributes = [(String, String)]

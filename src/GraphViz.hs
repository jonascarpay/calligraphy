{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GraphViz (render, pRenderConfig, RenderConfig) where

import Control.Monad
import Control.Monad.State
import Data.List (intercalate)
import Data.Tree (Tree (..))
import Options.Applicative hiding (style)
import Parse
import Printer
import Text.Show (showListWith)

data RenderConfig = RenderConfig
  { showCalls :: Bool,
    showLoc :: Bool,
    splines :: Bool,
    reverseDependencyRank :: Bool
  }

render :: RenderConfig -> Prints Modules
render RenderConfig {showCalls, splines, reverseDependencyRank, showLoc} (Modules modules calls) = do
  brack "digraph spaghetti {" "}" $ do
    unless splines $ textLn "splines=false;"
    textLn "node [style=filled fillcolor=\"#ffffffcf\"];"
    textLn "graph [outputorder=edgesfirst];"
    let nonEmptyModules = filter (not . null . snd) modules
    forM_ (zip nonEmptyModules [0 :: Int ..]) $
      \((modName, forest), ix) ->
        brack ("subgraph cluster_module_" <> show ix <> " {") "}" $ do
          strLn $ "label=" <> show modName <> ";"
          forM_ (zip forest [0 :: Int ..]) $ \(root, ix') -> do
            brack ("subgraph cluster_" <> show ix <> "_" <> show ix' <> " {") "}" $ do
              textLn "style=invis;"
              renderTreeNode root
    when showCalls $
      forM_ calls $ \(caller, callee) -> do
        if reverseDependencyRank
          then edge caller callee []
          else edge callee caller ["dir" .= "back"]
  where
    nodeLabel :: Decl -> String
    nodeLabel (Decl name _ _ _ loc)
      | showLoc = name <> "\n" <> show loc
      | otherwise = name
    renderTreeNode :: Prints (Tree Decl)
    renderTreeNode (Node decl@(Decl name key exported typ loc) children) = do
      strLn $ show (runKey key) <> " " <> style ["label" .= show (nodeLabel decl), "shape" .= nodeShape typ, "style" .= nodeStyle]
      forM_ children $ \child@(Node (Decl _ childKey _ _ _) _) -> do
        renderTreeNode child
        edge key childKey ["style" .= "dashed", "arrowhead" .= "none"]
      where
        nodeStyle :: String
        nodeStyle = show . intercalate ", " $
          flip execState [] $ do
            modify ("filled" :)
            unless exported $ modify ("dashed" :)
            when (typ == RecDecl) $ modify ("rounded" :)

nodeShape :: DeclType -> String
nodeShape DataDecl = "octagon"
nodeShape ConDecl = "box"
nodeShape RecDecl = "box"
nodeShape ClassDecl = "house"
nodeShape ValueDecl = "ellipse"

edge :: MonadPrint m => Key -> Key -> Style -> m ()
edge (Key from) (Key to) sty = strLn $ show from <> " -> " <> show to <> " " <> style sty

(.=) :: String -> String -> (String, String)
(.=) = (,)

style :: Style -> String
style sty = showListWith (\(key, val) -> showString key . showChar '=' . showString val) sty ";"

type Style = [(String, String)]

pRenderConfig :: Parser RenderConfig
pRenderConfig =
  RenderConfig
    <$> flag True False (long "hide-calls" <> help "Don't show function call arrows")
    <*> flag False True (long "show-loc" <> help "Show definition locations as line:col")
    <*> flag True False (long "no-splines" <> help "Render arrows as straight lines instead of splines")
    <*> flag False True (long "reverse-dependency-rank" <> short 'r' <> help "Make dependencies have lower rank than the dependee, i.e. show dependencies above their parent.")

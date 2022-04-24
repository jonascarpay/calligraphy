{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Calligraphy.Phases.Render (render, pRenderConfig, RenderConfig) where

import Calligraphy.Util.Printer
import Calligraphy.Util.Types
import Control.Monad
import qualified Data.EnumSet as EnumSet
import Data.List (intercalate)
import Data.Tree (Tree (..))
import Options.Applicative hiding (style)
import Text.Show (showListWith)

data RenderConfig = RenderConfig
  { showCalls :: Bool,
    showTypes :: Bool,
    showKey :: Bool,
    showGHCKeys :: Bool,
    showModulePath :: Bool,
    locMode :: LocMode,
    clusterModules :: Bool,
    clusterGroups :: Bool,
    splines :: Bool,
    reverseDependencyRank :: Bool
  }

render :: RenderConfig -> Prints CallGraph
render RenderConfig {..} (CallGraph modules calls types) = do
  brack "digraph calligraphy {" "}" $ do
    unless splines $ textLn "splines=false;"
    textLn "node [style=filled fillcolor=\"#ffffffcf\"];"
    textLn "graph [outputorder=edgesfirst];"
    let nonEmptyModules = filter (not . null . moduleForest) modules
    forM_ (zip nonEmptyModules [0 :: Int ..]) $
      \(Module modName modPath forest, moduleIx) ->
        moduleCluster moduleIx (if showModulePath then modPath else modName) $
          forM_ (zip forest [0 :: Int ..]) $ \(root, forestIx) -> do
            treeCluster moduleIx forestIx $
              renderTreeNode root
    when showCalls $
      forM_ calls $ \(caller, callee) ->
        if reverseDependencyRank
          then edge caller callee []
          else edge callee caller ["dir" .= "back"]
    when showTypes $
      forM_ types $ \(caller, callee) ->
        if reverseDependencyRank
          then edge caller callee ["style" .= "dotted"]
          else edge callee caller ["style" .= "dotted", "dir" .= "back"]
  where
    moduleCluster :: Int -> String -> Printer a -> Printer a
    moduleCluster modIx title inner
      | clusterModules =
          brack ("subgraph cluster_module_" <> show modIx <> " {") "}" $ do
            strLn $ "label=" <> show title <> ";"
            inner
      | otherwise = inner
    treeCluster :: Int -> Int -> Printer a -> Printer a
    treeCluster modIx groupIx inner
      | clusterGroups =
          brack ("subgraph cluster_" <> show modIx <> "_" <> show groupIx <> " {") "}" $ do
            textLn "style=invis;"
            inner
      | otherwise = inner
    nodeLabel :: Decl -> String
    nodeLabel (Decl name key ghcKeys _ _ loc) =
      intercalate "\n"
        . cons name
        . consIf showKey (show (unKey key))
        . consManyIf showGHCKeys (fmap (show . unGHCKey) (EnumSet.toList ghcKeys))
        . ( case locMode of
              Hide -> id
              Line -> cons ('L' : show (locLine loc))
              LineCol -> cons (show loc)
          )
        $ []

    renderTreeNode :: Prints (Tree Decl)
    renderTreeNode (Node decl@(Decl _ key _ exported typ _) children) = do
      strLn $ show (unKey key) <> " " <> style ["label" .= show (nodeLabel decl), "shape" .= nodeShape typ, "style" .= nodeStyle]
      forM_ children $ \child@(Node childDecl _) -> do
        renderTreeNode child
        edge key (declKey childDecl) ["style" .= "dashed", "arrowhead" .= "none"]
      where
        nodeStyle :: String
        nodeStyle =
          show . intercalate ", "
            . cons "filled"
            . consIf (not exported) "dashed"
            . consIf (typ == RecDecl) "rounded"
            $ []

cons :: a -> [a] -> [a]
cons = (:)

consIf :: Bool -> a -> [a] -> [a]
consIf True = (:)
consIf False = flip const

consManyIf :: Foldable f => Bool -> f a -> [a] -> [a]
consManyIf True fs as = foldr (:) as fs
consManyIf False _ as = as

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

data LocMode = Hide | Line | LineCol

pLocMode :: Parser LocMode
pLocMode =
  flag' Line (long "show-line" <> help "Show line numbers")
    <|> flag' LineCol (long "show-line-col" <> help "Show line and column numbers")
    <|> pure Hide

pRenderConfig :: Parser RenderConfig
pRenderConfig =
  RenderConfig
    <$> flag True False (long "hide-calls" <> help "Don't show call arrows")
    <*> flag True False (long "hide-types" <> help "Don't show type arrows")
    <*> flag False True (long "show-key" <> help "Show internal keys with identifiers. Useful for debugging.")
    <*> flag False True (long "show-ghc-key" <> help "Show GHC keys with identifiers. Useful for debugging.")
    <*> flag False True (long "show-module-path" <> help "Show a module's filepath instead of its name")
    <*> pLocMode
    <*> flag True False (long "no-cluster-modules" <> help "Don't draw modules as a cluster.")
    <*> flag True False (long "no-cluster-trees" <> help "Don't draw definition trees as a cluster.")
    <*> flag True False (long "no-splines" <> help "Render arrows as straight lines instead of splines")
    <*> flag False True (long "reverse-dependency-rank" <> help "Make dependencies have lower rank than the dependee, i.e. show dependencies above their parent.")

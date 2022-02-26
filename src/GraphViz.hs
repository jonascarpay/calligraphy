{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module GraphViz (render) where

import Config (RenderConfig (..))
import Control.Monad
import Control.Monad.State
import Data.EnumMap (EnumMap)
import Data.EnumMap qualified as EnumMap
import Data.EnumSet (EnumSet)
import Data.EnumSet qualified as EnumSet
import Data.List (intercalate)
import Data.Map qualified as Map
import Parse
import Printer
import Text.Show (showListWith)

newtype Key = Key {runKey :: Int}

data DrawState = DrawState
  { representative :: !(EnumMap GHCKey Key),
    fresh :: !Int
  }

render :: RenderConfig -> Prints [Module]
render RenderConfig {..} modules = do
  brack "digraph spaghetti {" "}" $ do
    unless splines $ textLn "splines=false;"
    textLn "node [style=filled fillcolor=\"#ffffffcf\"];"
    textLn "graph [outputorder=edgesfirst];"
    DrawState reps _ <- flip execStateT (DrawState mempty 0) $
      forM_ (zip modules [0 :: Int ..]) $
        \(Module modName exports (SemanticTree tree) _, ix) ->
          brack ("subgraph cluster_module_" <> show ix <> " {") "}" $ do
            strLn $ "label=" <> show modName <> ";"
            forM_ (Map.toList tree) $ \node -> do
              Key nCluster <- tick
              brack ("subgraph cluster_" <> show nCluster <> " {") "}" $ do
                textLn "style=invis;"
                tick >>= renderTreeNode exports node
    forM_ (modules >>= EnumMap.toList . modCalls >>= traverse EnumSet.toList) $ \(caller, callee) -> sequence_ $ do
      nCaller <- EnumMap.lookup caller reps
      nCallee <- EnumMap.lookup callee reps
      pure $
        if reverseDependencyRank
          then edge nCaller nCallee []
          else edge nCallee nCaller ["dir" .= "back"]
  where
    tellRep :: GHCKey -> Key -> StateT DrawState Printer ()
    tellRep key rep = modify $ \(DrawState r n) -> DrawState (EnumMap.insert key rep r) n
    tick :: StateT DrawState Printer Key
    tick = state $ \(DrawState r n) -> (Key n, DrawState r (n + 1))

    renderTreeNode :: EnumSet GHCKey -> (String, (EnumSet GHCKey, DeclType, SemanticTree)) -> Key -> StateT DrawState Printer ()
    renderTreeNode exports (str, (keys, typ, SemanticTree sub)) this = do
      forM_ (EnumSet.toList keys) $ \k -> tellRep k this
      -- TODO `node`
      strLn $ show (runKey this) <> " " <> style ["label" .= show str, "shape" .= nodeShape typ, "style" .= nodeStyle]
      forM_ (Map.toList sub) $ \child -> do
        nChild <- tick
        renderTreeNode exports child nChild
        edge this nChild ["style" .= "dashed", "arrowhead" .= "none"]
      where
        nodeStyle :: String
        nodeStyle = show . intercalate ", " $
          flip execState [] $ do
            modify ("filled" :)
            unless (any (flip EnumSet.member exports) (EnumSet.toList keys)) $ modify ("dashed" :)
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

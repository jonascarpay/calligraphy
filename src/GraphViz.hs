{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GraphViz (render) where

import Control.Monad
import Control.Monad.State
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Parse
import Printer
import Text.Show (showListWith)

data DrawState = DrawState
  { representative :: !(IntMap Int),
    fresh :: !Int
  }

render :: Prints [Module]
render modules = do
  brack "digraph spaghetti {" "}" $ do
    -- textLn "splines=false;"
    textLn "node [style=filled fillcolor=\"#ffffffcf\"];"
    textLn "graph [outputorder=edgesfirst];"
    DrawState reps _ <- flip execStateT (DrawState mempty 0) $
      forM_ (zip modules [0 :: Int ..]) $
        \(Module modName exports (SemanticTree tree) _, ix) ->
          brack ("subgraph cluster_module_" <> show ix <> " {") "}" $ do
            strLn $ "label=" <> show modName <> ";"
            forM_ (Map.toList tree) $ \node -> do
              nCluster <- tick
              brack ("subgraph cluster_" <> show nCluster <> " {") "}" $ do
                textLn "style=invis;"
                tick >>= renderTreeNode exports node
    forM_ (modules >>= Set.toList . modCalls) $ \(caller, callee) -> sequence_ $ do
      nCaller <- reps IntMap.!? caller
      nCallee <- reps IntMap.!? callee
      pure $ edge nCallee nCaller ["dir" .= "back"]
  where
    tellRep :: Int -> Int -> StateT DrawState Printer ()
    tellRep key rep = modify $ \(DrawState r n) -> DrawState (IntMap.insert key rep r) n
    tick :: StateT DrawState Printer Int
    tick = state $ \(DrawState r n) -> (n, DrawState r (n + 1))

    renderTreeNode :: IntSet -> (String, (IntSet, DeclType, SemanticTree)) -> Int -> StateT DrawState Printer ()
    renderTreeNode exports (str, (keys, typ, SemanticTree sub)) this = do
      forM_ (IntSet.toList keys) $ \k -> tellRep k this
      strLn $ show this <> " " <> style ["label" .= show str, "shape" .= nodeShape typ, "style" .= nodeStyle]
      forM_ (Map.toList sub) $ \child -> do
        nChild <- tick
        renderTreeNode exports child nChild
        edge this nChild ["style" .= "dashed", "arrowhead" .= "none"]
      where
        nodeStyle :: String
        nodeStyle = show . intercalate ", " $
          flip execState [] $ do
            modify ("filled" :)
            unless (any (flip IntSet.member exports) (IntSet.toList keys)) $ modify ("dashed" :)
            when (typ == RecDecl) $ modify ("rounded" :)
        nodeShape :: DeclType -> String
        nodeShape DataDecl = "octagon"
        nodeShape ConDecl = "box"
        nodeShape RecDecl = "box"
        nodeShape ClassDecl = "house"
        nodeShape ValueDecl = "ellipse"

edge :: MonadPrint m => Int -> Int -> Style -> m ()
edge from to sty = strLn $ show from <> " -> " <> show to <> " " <> style sty

(.=) :: String -> String -> (String, String)
(.=) = (,)

style :: Style -> String
style sty = showListWith (\(key, val) -> showString key . showChar '=' . showString val) sty ";"

type Style = [(String, String)]

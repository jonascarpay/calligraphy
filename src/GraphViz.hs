{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GraphViz (render) where

import Config qualified as Cfg
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Set (Set)
import Data.Set qualified as S
import Parse
import Printer

treeKey :: DeclTree -> Int
treeKey (DeclTree _ (Name k _) _ _) = k

ppNode :: DeclTree -> StateT DrawState Printer ()
ppNode (DeclTree typ (Name key str) (Use calls) children') = do
  DrawState ns ss cs <- get
  unless (IS.member key ns) $ do
    let children = unScope children'
    put $
      DrawState
        (IS.insert key ns)
        (IM.insert key (IS.fromList $ treeKey <$> children) ss)
        (IM.insert key calls cs)
    strLn $ show key <> " [label=" <> show str <> "];"
    mapM_ ppNode children

data DrawState = DrawState
  { drawnNodes :: IntSet,
    subs :: IntMap IntSet,
    calls :: IntMap IntSet
  }

{-# INLINE foldMapM #-}
foldMapM :: (Monoid b, Monad m, Foldable f) => (a -> m b) -> f a -> m b
foldMapM f xs = foldr step return xs mempty
  where
    step x r z = f x >>= \y -> r $! z `mappend` y

{-# INLINE foldForM #-}
foldForM :: (Monoid b, Monad m, Foldable f) => f a -> (a -> m b) -> m b
foldForM = flip foldMapM

forEdges :: Monad m => IntSet -> IntMap IntSet -> (Int -> Int -> m ()) -> m ()
forEdges drawn edges k = forM_ (IM.toList edges) $ \(from, tos) ->
  when (IS.member from drawn) . forM_ (IS.toList tos) $ \to ->
    when (IS.member to drawn) $
      k from to

render :: Prints [Module]
render modules = brack "digraph {" "}" $ do
  textLn "splines=false;"
  textLn "graph [overlap=false];"
  (ns, cs) <- foldForM (zip modules [0 :: Int ..]) $
    \(Module modname nodes, i) -> do
      brack ("subgraph cluster_" <> show i <> " {") "}" $ do
        strLn $ "label=" <> modname <> ";"
        DrawState ns ss cs <- execStateT (mapM_ ppNode nodes) (DrawState mempty mempty mempty)
        forEdges ns ss $ \from to -> strLn $ show from <> " -> " <> show to <> " [style=dashed, arrowhead=none, weight=100];"
        pure (ns, cs)
  forEdges ns cs $ \from to -> strLn $ show from <> " -> " <> show to <> ";"

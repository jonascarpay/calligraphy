{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GraphViz (render) where

import Control.Monad
import Control.Monad.State
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (intercalate)
import Parse
import Printer
import Text.Show (showListWith)

render :: a
render = undefined

{-
render :: Prints [Module]
render modules = do
  brack "digraph spaghetti {" "}" $ do
    textLn "splines=false;"
    textLn "node [fillcolor=\"#ffffffcf\"];"
    textLn "graph [overlap=false, outputorder=edgesfirst];"
    -- textLn "graph [overlap=false];"
    -- textLn "newrank=true"
    (ns, cs) <- foldForM (zip modules [0 :: Int ..]) $
      \(Module modname exports tree calls, i) -> do
        brack ("subgraph cluster_" <> show i <> " {") "}" $ do
          strLn $ "label=" <> modname <> ";"
          foldForM (zip nodes [0 :: Int ..]) $ \(node, iNode) -> brack ("subgraph cluster_" <> show i <> "_" <> show iNode <> " {") "}" $ do
            textLn "style=invis;"
            DrawState ns ss cs <-
              flip execStateT (DrawState mempty mempty mempty) $
                ppNode exports node
            forEdges ns ss $ \from to -> strLn $ show from <> " -> " <> show to <> "[style=dashed, arrowhead=none];"
            pure (ns, cs)
    forEdges ns cs $ \from to -> strLn $ show from <> " -> " <> show to <> ";"

data DrawState = DrawState
  { _drawnNodes :: IntSet,
    _subs :: IntMap IntSet,
    _calls :: IntMap IntSet
  }

type Style = [(String, String)]

style :: Style -> String
style sty = showListWith (\(key, val) -> showString key . showChar '=' . showString val) sty ";"

(.=) :: String -> String -> (String, String)
(.=) = (,)

{-# INLINE foldMapM #-}
foldMapM :: (Monoid b, Monad m, Foldable f) => (a -> m b) -> f a -> m b
foldMapM f xs = foldr step return xs mempty
  where
    step x r z = f x >>= \y -> r $! z `mappend` y

{-# INLINE foldForM #-}
foldForM :: (Monoid b, Monad m, Foldable f) => f a -> (a -> m b) -> m b
foldForM = flip foldMapM

treeKey :: DeclTree -> Int
treeKey (DeclTree _ (Name k _) _ _) = k

forEdges :: Monad m => IntSet -> IntMap IntSet -> (Int -> Int -> m ()) -> m ()
forEdges drawn edges k = forM_ (IM.toList edges) $ \(from, tos) ->
  when (IS.member from drawn) . forM_ (IS.toList tos) $ \to ->
    when (IS.member to drawn) $
      k from to

    -}

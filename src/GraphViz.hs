{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GraphViz (render) where

import Config
import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Set (Set)
import Data.Set qualified as S
import Lib
import Printer (Printer, indent, strLn, textLn)

type CallWriter = State (Set (Int, Int))

tell :: Set (Int, Int) -> CallWriter ()
tell = modify . mappend

cowSaysA :: Int
cowSaysA = cowSaysB
  where
    cowSaysB = cowSaysC
    cowSaysC = cowSaysA

render :: RenderConfig -> [ParsedModule] -> Printer ()
render cfg modulesUnfiltered = do
  let (RenderGraph modules calls) = toRenderGraph cfg modulesUnfiltered
  textLn "digraph {"
  indent $ do
    unless (splines cfg) $ strLn "splines=false;"
    textLn "graph [overlap=false];"
    textLn "// Module Clusters"
    forM_ (zip modules [0 :: Int ..]) $ \(RenderModule name nodes, i) -> do
      strLn $ "subgraph cluster_" <> show i <> " {"
      indent $ do
        strLn $ "label=" <> show name <> ";"
        mapM_ renderNode nodes
      textLn "}"
    when (showCalls cfg) $ do
      textLn "// Call graph"
      forM_ calls $ \(caller, callee) -> strLn $ show caller <> " -> " <> show callee <> ";"
  textLn "}"
  where
    style ScopeLocal = "style=dashed"
    style ScopeExport = "shape=diamond"
    style _ = mempty
    renderNode :: RenderNode -> Printer ()
    renderNode (RenderNode name key scope subs) = do
      strLn $ show key <> " [label=" <> show name <> ", " <> style scope <> "];"
      indent $ mapM_ renderNode subs

toRenderGraph :: RenderConfig -> [ParsedModule] -> RenderGraph
toRenderGraph (RenderConfig renderLevel showCalls _splines includeFilters excludeFilters) modulesUnfiltered =
  RenderGraph renderedModules (if showCalls then filter (flip IS.member renderedNodes . snd) $ S.toList calls else [])
  where
    modules = filter (\m -> isNotExcluded excludeFilters m && isIncluded includeFilters m) modulesUnfiltered
    (renderedModules, calls) = runState (mapM (renderModule renderLevel) modules) mempty
    renderedNodes :: IntSet
    renderedNodes = foldMap (foldMap f . rmDecls) renderedModules
      where
        f (RenderNode _ key _ subs) = IS.singleton key <> foldMap f subs

isIncluded :: [Filter] -> ParsedModule -> Bool
isIncluded [] _ = True
isIncluded fs p = any (flip match $ pmName p) fs

isNotExcluded :: [Filter] -> ParsedModule -> Bool
isNotExcluded [] _ = True
isNotExcluded fs p = not $ any (flip match $ pmName p) fs

renderModule :: RenderLevel -> ParsedModule -> CallWriter RenderModule
renderModule renderLevel (ParsedModule name exports binds) = do
  nodes' <- sequence $ binds >>= toList . renderBind
  pure $ RenderModule name nodes'
  where
    renderBind :: Declaration -> Maybe (CallWriter RenderNode)
    renderBind self@Declaration {declName, declKey, declSubs, declCalls} = do
      let scope = if IS.member declKey exports then ScopeExport else ScopeModule
       in case renderLevel of
            Exports | scope /= ScopeExport -> Nothing
            All ->
              Just $ do
                let goSubs :: Declaration -> CallWriter RenderNode
                    goSubs Declaration {declName, declKey, declSubs, declCalls} = do
                      modify $ mappend $ directCalls declKey declCalls
                      RenderNode declName declKey ScopeLocal <$> mapM goSubs declSubs
                tell $ directCalls declKey declCalls
                RenderNode declName declKey scope <$> mapM goSubs declSubs
            _ ->
              Just
                (RenderNode declName declKey scope [] <$ collectCalls self)

    directCalls :: Int -> IntSet -> Set (Int, Int)
    directCalls caller = S.fromList . fmap (caller,) . IS.toList
    collectCalls :: Declaration -> CallWriter ()
    collectCalls self@Declaration {declKey = caller} = go self
      where
        go Declaration {declCalls, declSubs} = do
          tell $ directCalls caller (IS.delete caller declCalls)
          mapM_ go declSubs

data RenderGraph = RenderGraph
  { rgModules :: [RenderModule],
    rgCalls :: [(Int, Int)]
  }

data RenderModule = RenderModule
  { rmName :: String,
    rmDecls :: [RenderNode]
  }

data NodeScope
  = ScopeLocal
  | ScopeModule
  | ScopeExport
  deriving (Eq, Show)

data RenderNode = RenderNode
  { rnName :: String,
    rnKey :: Int,
    rnScope :: NodeScope,
    rnSubs :: [RenderNode]
  }

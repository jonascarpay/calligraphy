{-# LANGUAGE ScopedTypeVariables #-}

module Calligraphy.Phases.NodeFilter (filterNodes, FilterConfig, pFilterConfig, FilterError (..)) where

import Calligraphy.Phases.Parse
import Control.Monad.State
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as EnumSet
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree
import Data.Tuple (swap)
import Options.Applicative
import Prelude hiding (filter)

-- TODO
-- hide-classes
-- hide-values
-- hide-data
-- etc
data FilterConfig = FilterConfig
  { _onlyExports :: Bool,
    _depRoot :: Maybe (NonEmpty String),
    _revDepRoot :: Maybe (NonEmpty String),
    _depDepth :: Maybe Int
  }

filterNodes :: FilterConfig -> Modules -> Either FilterError Modules
filterNodes fcfg@(FilterConfig exps _ _ _) mods = do
  depFilter <- dependencyFilter fcfg mods
  let p decl = exportFilter decl && depFilter decl
  pure $ filterModules p mods
  where
    exportFilter :: Decl -> Bool
    exportFilter
      | exps = declExported
      | otherwise = const True

resolveNames :: Forest Decl -> Map String (EnumSet Key)
resolveNames forest =
  flip execState mempty $
    flip (traverse . traverse) forest $
      \(Decl name key _ _ _) -> modify (Map.insertWith (<>) name (EnumSet.singleton key))

transitives :: forall a. Enum a => Maybe Int -> [a] -> Set (a, a) -> EnumSet a
transitives maxDepth roots deps = go 0 mempty (EnumSet.fromList roots)
  where
    go :: Int -> EnumSet a -> EnumSet a -> EnumSet a
    go depth old new
      | EnumSet.null new = old
      | maybe False (< depth) maxDepth = old
      | otherwise =
        let old' = old <> new
            new' = EnumSet.foldr (\a -> maybe id mappend $ EnumMap.lookup a adjacencies) mempty new
         in go (depth + 1) old' (new' EnumSet.\\ old')
    adjacencies :: EnumMap a (EnumSet a)
    adjacencies = foldr (\(from, to) -> EnumMap.insertWith (<>) from (EnumSet.singleton to)) mempty deps

newtype FilterError = UnknownRootName String

dependencyFilter :: FilterConfig -> Modules -> Either FilterError (Decl -> Bool)
dependencyFilter (FilterConfig _ mfw mbw maxDepth) (Modules modules calls infers) = do
  fwFilter <- forM mfw $ flip mkDepFilter (calls <> infers)
  bwFilter <- forM mbw $ flip mkDepFilter (Set.map swap (calls <> infers))
  pure $ case (fwFilter, bwFilter) of
    (Nothing, Nothing) -> const True
    (Just fa, Nothing) -> fa
    (Nothing, Just fb) -> fb
    (Just fa, Just fb) -> \decl -> fa decl || fb decl
  where
    names :: Map String (EnumSet Key)
    names = resolveNames (modules >>= snd)
    mkDepFilter :: NonEmpty String -> Set (Key, Key) -> Either FilterError (Decl -> Bool)
    mkDepFilter rootNames edges = do
      rootKeys <- forM rootNames $ \name -> maybe (Left $ UnknownRootName name) (pure . EnumSet.toList) (Map.lookup name names)
      let ins = transitives maxDepth (mconcat $ toList rootKeys) edges
      pure $ \decl -> EnumSet.member (declKey decl) ins

filterModules :: (Decl -> Bool) -> Modules -> Modules
filterModules p (Modules modules calls infers) = Modules modules' calls' infers'
  where
    (modules', outputKeys) = runState ((traverse . traverse) filterForest modules) mempty
    filterForest :: Forest Decl -> State (EnumSet Key) (Forest Decl)
    filterForest = fmap catMaybes . mapM filterTree
    filterTree :: Tree Decl -> State (EnumSet Key) (Maybe (Tree Decl))
    filterTree (Node decl children) = do
      children' <- filterForest children
      if not (p decl) && null children'
        then pure Nothing
        else Just (Node decl children') <$ modify (EnumSet.insert (declKey decl))
    calls' = Set.filter (\(a, b) -> EnumSet.member a outputKeys && EnumSet.member b outputKeys) calls
    infers' = Set.filter (\(a, b) -> EnumSet.member a outputKeys && EnumSet.member b outputKeys) infers

pFilterConfig :: Parser FilterConfig
pFilterConfig =
  FilterConfig
    <$> switch (long "hide-local-bindings" <> long "exports-only" <> help "Don't draw non-exported bindings.")
    <*> (fmap nonEmpty . many)
      ( strOption
          ( long "forward-root"
              <> short 'f'
              <> metavar "NAME"
              <> help "Dependency filter root. Will hide everything that's not a (transitive) dependency of this root. Can be repeated."
          )
      )
    <*> (fmap nonEmpty . many)
      ( strOption
          ( long "reverse-root"
              <> short 'r'
              <> metavar "NAME"
              <> help "Reverse dependency filter root. Will hide everything that's not a (transitive) reverse dependency of this root. Can be repeated."
          )
      )
    <*> optional (option auto (long "max-depth" <> help "Maximum search depth for transitive dependencies."))

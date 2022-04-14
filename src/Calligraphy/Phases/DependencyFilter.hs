{-# LANGUAGE ScopedTypeVariables #-}

module Calligraphy.Phases.DependencyFilter
  ( DependencyFilterConfig,
    DependencyFilterError (..),
    ppFilterError,
    dependencyFilter,
    pDependencyFilterConfig,
  )
where

import Calligraphy.Util.Printer
import Calligraphy.Util.Types
import Control.Monad.State
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as EnumSet
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree
import Data.Tuple (swap)
import Options.Applicative
import Prelude hiding (filter)

data DependencyFilterConfig = DependencyFilterConfig
  { _depRoot :: Maybe (NonEmpty String),
    _revDepRoot :: Maybe (NonEmpty String),
    _depDepth :: Maybe Int
  }

pDependencyFilterConfig :: Parser DependencyFilterConfig
pDependencyFilterConfig =
  DependencyFilterConfig
    <$> (fmap nonEmpty . many)
      ( strOption
          ( long "forward-root"
              <> short 'f'
              <> metavar "NAME"
              <> help "Name of a dependency filter root. Specifying a dependecy filter root hides everything that's not a (transitive) dependency of a root. The name can be qualified. This argument can be repeated."
          )
      )
    <*> (fmap nonEmpty . many)
      ( strOption
          ( long "reverse-root"
              <> short 'r'
              <> metavar "NAME"
              <> help "Name of a reverse dependency filter root. Specifying a dependecy filter root hides everything that's not a (transitive) reverse dependency of a root. The name can be qualified. This argument can be repeated."
          )
      )
    <*> optional (option auto (long "max-depth" <> help "Maximum search depth for transitive dependencies."))

newtype DependencyFilterError = UnknownRootName String

ppFilterError :: Prints DependencyFilterError
ppFilterError (UnknownRootName root) = strLn $ "Unknown root name: " <> root

-- | If p holds, that node, and all its incestors are included in the result.
-- Compare this to 'filterModules', where a node is included only if p holds for it and all ancestors.
pruneModules :: (Decl -> Bool) -> Modules -> Modules
pruneModules p (Modules modules calls infers) = removeDeadCalls $ Modules modules' calls infers
  where
    modules' = (fmap . fmap) pruneForest modules
    pruneForest :: Forest Decl -> Forest Decl
    pruneForest = mapMaybe pruneTree
    pruneTree :: Tree Decl -> Maybe (Tree Decl)
    pruneTree (Node decl children) = do
      let children' = pruneForest children
       in if not (p decl) && null children'
            then Nothing
            else Just (Node decl children')

dependencyFilter :: DependencyFilterConfig -> Modules -> Either DependencyFilterError Modules
dependencyFilter (DependencyFilterConfig mfw mbw maxDepth) mods@(Modules modules calls infers) = do
  fwFilter <- forM mfw $ flip mkDepFilter (calls <> infers)
  bwFilter <- forM mbw $ flip mkDepFilter (Set.map swap (calls <> infers))
  pure $
    let p = case (fwFilter, bwFilter) of
          (Nothing, Nothing) -> const True
          (Just fa, Nothing) -> fa
          (Nothing, Just fb) -> fb
          (Just fa, Just fb) -> \decl -> fa decl || fb decl
     in pruneModules p mods
  where
    names :: Map String (EnumSet Key)
    names = Map.unionsWith mappend (fmap (uncurry resolveNames) modules)
    mkDepFilter :: NonEmpty String -> Set (Key, Key) -> Either DependencyFilterError (Decl -> Bool)
    mkDepFilter rootNames edges = do
      rootKeys <- forM rootNames $ \name -> maybe (Left $ UnknownRootName name) (pure . EnumSet.toList) (Map.lookup name names)
      let ins = transitives maxDepth (mconcat $ toList rootKeys) edges
      pure $ \decl -> EnumSet.member (declKey decl) ins

-- | Create a map of all names, and the keys they correspond to.
-- For every name in the source, this introduces two entries; one naked, and one qualified with the module name.
resolveNames :: String -> Forest Decl -> Map String (EnumSet Key)
resolveNames modName forest =
  flip execState mempty $
    flip (traverse . traverse) forest $
      \(Decl name key _ _ _) ->
        modify $
          Map.insertWith (<>) (modName <> "." <> name) (EnumSet.singleton key)
            . Map.insertWith (<>) name (EnumSet.singleton key)

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

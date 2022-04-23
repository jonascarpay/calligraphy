{-# LANGUAGE ScopedTypeVariables #-}

module Calligraphy.Phases.DependencyFilter
  ( DependencyFilterConfig,
    DependencyFilterError (..),
    ppFilterError,
    dependencyFilter,
    pDependencyFilterConfig,
  )
where

import Calligraphy.Util.Optparse (boolFlags)
import Calligraphy.Util.Printer
import Calligraphy.Util.Types
import Control.Monad.State.Strict
import Data.Bifunctor (bimap)
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
    _depDepth :: Maybe Int,
    _followParent :: Bool,
    _followChildren :: Bool,
    _followCalls :: Bool,
    _followTypes :: Bool
  }

pDependencyFilterConfig :: Parser DependencyFilterConfig
pDependencyFilterConfig =
  DependencyFilterConfig
    <$> (fmap nonEmpty . many)
      ( strOption
          ( long "forward-root"
              <> short 'f'
              <> metavar "NAME"
              <> help "Name of a dependency filter root. Specifying a dependency filter root hides everything that's not a (transitive) dependency of a root. The name can be qualified. This argument can be repeated."
          )
      )
    <*> (fmap nonEmpty . many)
      ( strOption
          ( long "reverse-root"
              <> short 'r'
              <> metavar "NAME"
              <> help "Name of a reverse dependency filter root. Specifying a dependency filter root hides everything that's not a (transitive) reverse dependency of a root. The name can be qualified. This argument can be repeated."
          )
      )
    <*> optional (option auto (long "max-depth" <> help "Maximum search depth for transitive dependencies."))
    <*> boolFlags True "parent-deps" "When calculating transitive dependencies, follow edges to a parents." mempty
    <*> boolFlags True "children-deps" "When calculating transitive dependencies, follow edges to children." mempty
    <*> boolFlags True "value-deps" "When calculating transitive dependencies, follow value edges." mempty
    <*> boolFlags False "type-deps" "When calculating transitive dependencies, follow type edges." mempty

newtype DependencyFilterError = UnknownRootName String

ppFilterError :: Prints DependencyFilterError
ppFilterError (UnknownRootName root) = strLn $ "Unknown root name: " <> root

-- | If p holds, that node, and all its incestors are included in the result.
-- Compare this to 'filterModules', where a node is included only if p holds for it and all ancestors.
pruneModules :: (Decl -> Bool) -> Modules -> Modules
pruneModules p (Modules modules calls types) = removeDeadCalls $ Modules modules' calls types
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
dependencyFilter (DependencyFilterConfig mfw mbw maxDepth useParent useChild useCalls useTypes) mods@(Modules modules calls types) = do
  fwFilter <- forM mfw $ flip mkDepFilter edges
  bwFilter <- forM mbw $ flip mkDepFilter (Set.map swap edges)
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

    edges =
      mconcat
        [ if useParent then parentEdges else mempty,
          if useChild then childEdges else mempty,
          if useCalls then calls else mempty,
          if useTypes then types else mempty
        ]

    parentEdges, childEdges :: Set (Key, Key)
    (parentEdges, childEdges) = execState ((traverse . traverse . traverse) go modules) mempty
      where
        go :: Tree Decl -> State (Set (Key, Key), Set (Key, Key)) ()
        go (Node parent children) =
          forM_ children $ \childNode@(Node child _) -> do
            let kParent = declKey parent
                kChild = declKey child
            modify $ bimap (Set.insert (kParent, kChild)) (Set.insert (kChild, kParent))
            go childNode

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

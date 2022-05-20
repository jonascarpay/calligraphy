{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Calligraphy.Util.Types
  ( -- * Data types
    CallGraph (..),
    Module (..),
    Decl (..),
    DeclType (..),
    Key (..),
    GHCKey (..),
    Loc (..),

    -- * Utility functions
    rekeyCalls,
    ppCallGraph,
    moduleTree,
    encodeModuleTree,

    -- * Lensy stuff
    over,
    forT_,
    modForest,
    modDecls,
    forestT,
  )
where

import Calligraphy.Util.Lens
import Calligraphy.Util.Printer
import Control.Monad
import Control.Monad.State (evalState, state)
import qualified Data.Aeson as Aeson
import Data.Bitraversable (bitraverse)
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.EnumSet (EnumSet)
import Data.Graph
import Data.List (isPrefixOf)
import Data.Semigroup (Max (Max))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Tree (rootLabel)

-- | This is the main type that processing phases will operate on.
-- Note that calls and typing judgments are part of this top-level structure, not of the individual modules.
data CallGraph = CallGraph
  { _modules :: [Module],
    _calls :: Set (Key, Key),
    _types :: Set (Key, Key)
  }

data Module = Module
  { moduleName :: String,
    modulePath :: FilePath,
    moduleForest :: Forest Decl
  }

data Decl = Decl
  { declName :: String,
    declKey :: Key,
    declGHCKeys :: EnumSet GHCKey,
    declExported :: Bool,
    declType :: DeclType,
    declLoc :: Loc
  }

-- FIXME: Ord is too general and doesn't really fit the "is child of" property nicely
-- Partition forest based on the value of the root nodes
partitionForest :: Ord b => (a -> b) -> a -> Forest a -> (Forest a, Forest a, Forest a)
partitionForest _ _ [] = ([], [], [])
partitionForest f a (a' : as) =
  let (smaller, eq, bigger) = partitionForest f a as
   in case compare (f a) (f $ rootLabel a') of
        LT -> (a' : smaller, eq, bigger)
        EQ -> (smaller, a' : eq, bigger)
        GT -> (smaller, eq, a' : bigger)

-- build a tree such that parents < children
buildOrdTree :: Ord b => (v -> b) -> [v] -> Forest v
buildOrdTree f = insertForest []
  where
    insertForest forest [] = forest
    insertForest forest (v : vs) =
      case partitionForest f v forest of
        (smaller, eq, []) -> insertForest (Node v smaller : eq) vs
        ([], eq, [bigger]) -> insertForest (eq <> [insertInto v bigger]) vs
        (_, _, _) -> error "invariant in buildOrdTree broken"
    insertInto v (Node v' cs) = Node v' (insertForest cs [v])

newtype Hierachy = Hierachy [String]

instance Eq Hierachy where
  a == b = compare a b == EQ

instance Ord Hierachy where
  compare (Hierachy xs) (Hierachy ys)
    | xs == ys = EQ
    | xs `isPrefixOf` ys = LT
    | ys `isPrefixOf` xs = GT
    | otherwise = EQ

moduleTree :: CallGraph -> Tree (Key, Module)
moduleTree (CallGraph modules _ _) = evalState (addKeys tree) (succ startKey)
  where
    tree = case forest of
      [t] -> t
      _ -> Node rootModule forest
    rootModule = Module "" "" []
    forest = buildOrdTree moduleHierarchy modules
    Max startKey = foldMap (foldMap (foldMap (Max . declKey)) . moduleForest) modules
    addKeys (Node a children) = do
      c <- traverse addKeys children
      k <- nextKey
      pure $ Node (k, a) c
      where
        nextKey = state $ \k -> (k, succ k)

encodeModuleTree :: Tree (Key, Module) -> Aeson.Value
encodeModuleTree (Node (k, Module name fp decl) ch) =
  let children = fmap encodeModuleTree ch <> fmap encodeDeclTree decl
   in Aeson.object
        [ (fromString "tag", fromString "module"),
          (fromString "key", Aeson.toJSON $ unKey k),
          (fromString "name", Aeson.toJSON name),
          (fromString "filePath", Aeson.toJSON fp),
          (fromString "children", Aeson.toJSON children)
        ]

encodeDeclTree :: Tree Decl -> Aeson.Value
encodeDeclTree (Node decl ch) =
  let children = fmap encodeDeclTree ch
   in Aeson.object
        [ (fromString "tag", fromString "declaration"),
          (fromString "name", fromString . declName $ decl),
          (fromString "key", Aeson.toJSON . unKey . declKey $ decl),
          (fromString "exported", Aeson.toJSON . declExported $ decl),
          (fromString "type", fromString . declTypeString . declType $ decl),
          (fromString "location", encodeLoc . declLoc $ decl),
          (fromString "children", Aeson.toJSON children)
        ]
  where
    encodeLoc (Loc l c) =
      Aeson.object
        [ (fromString "line", Aeson.toJSON l),
          (fromString "column", Aeson.toJSON c)
        ]
    declTypeString ValueDecl = "value"
    declTypeString RecDecl = "record"
    declTypeString ConDecl = "constructor"
    declTypeString DataDecl = "data"
    declTypeString ClassDecl = "type-class"

moduleHierarchy :: Module -> Hierachy
moduleHierarchy = Hierachy . splitDot . moduleName
  where
    splitDot str =
      let (pre, post) = break (== '.') str
       in case post of
            [] -> [pre]
            (_ : ys) -> pre : splitDot ys

-- | A key in our own local space, c.f. a key that was generated by GHC.
newtype Key = Key {unKey :: Int}
  deriving (Enum, Show, Eq, Ord, Bounded)

-- | A key that was produced by GHC, c.f. Key that we produced ourselves.
-- We wrap it in a newtype because GHC itself uses a type synonym, but we want conversions to be as explicit as possible.
newtype GHCKey = GHCKey {unGHCKey :: Int}
  deriving newtype (Show, Enum, Eq, Ord)

data DeclType
  = ValueDecl
  | RecDecl
  | ConDecl
  | DataDecl
  | ClassDecl
  deriving
    (Eq, Ord, Show)

data Loc = Loc
  { locLine :: !Int,
    locCol :: !Int
  }
  deriving (Eq, Ord)

instance Show Loc where
  showsPrec _ (Loc ln col) = shows ln . showChar ':' . shows col

{-# INLINE modDecls #-}
modDecls :: Traversal' Module Decl
modDecls = modForest . forestT

{-# INLINE modForest #-}
modForest :: Traversal' Module (Forest Decl)
modForest f (Module nm fp ds) = Module nm fp <$> f ds

{-# INLINE forestT #-}
forestT :: Traversal (Forest a) (Forest b) a b
forestT = traverse . traverse

rekeyCalls :: (Enum a, Ord b) => EnumMap a b -> Set (a, a) -> Set (b, b)
rekeyCalls m = foldr (maybe id Set.insert . bitraverse (flip EnumMap.lookup m) (flip EnumMap.lookup m)) mempty

ppCallGraph :: Prints CallGraph
ppCallGraph (CallGraph modules _ _) = forM_ modules $ \(Module modName modPath forest) -> do
  strLn $ modName <> " (" <> modPath <> ")"
  indent $ mapM_ ppTree forest

ppTree :: Prints (Tree Decl)
ppTree (Node (Decl name _key _ghckey _exp typ loc) children) = do
  strLn $ name <> " (" <> show typ <> ", " <> show loc <> ")"
  indent $ mapM_ ppTree children

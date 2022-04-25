{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

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

    -- * Lensy stuff
    over,
    forT_,
    modForest,
    modDecls,
  )
where

import Calligraphy.Util.Printer
import Control.Monad
import Control.Monad.Identity
import Data.Bitraversable (bitraverse)
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.EnumSet (EnumSet)
import Data.Graph
import Data.Set (Set)
import qualified Data.Set as Set

-- | This is the main type that phases will operate on.
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

newtype Key = Key {unKey :: Int}
  deriving (Enum, Show, Eq, Ord)

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
  { locLine :: Int,
    locCol :: Int
  }

type Traversal s t a b = forall m. Applicative m => (a -> m b) -> (s -> m t)

type Traversal' s a = Traversal s s a a

{-# INLINE modDecls #-}
modDecls :: Traversal' Module Decl
modDecls = modForest . traverse . traverse

{-# INLINE modForest #-}
modForest :: Traversal' Module (Forest Decl)
modForest f (Module nm fp ds) = Module nm fp <$> f ds

newtype ConstT m a = ConstT {unConstT :: m ()}
  deriving (Functor)

instance Applicative m => Applicative (ConstT m) where
  {-# INLINE pure #-}
  pure _ = ConstT (pure ())
  {-# INLINE (<*>) #-}
  ConstT mf <*> ConstT ma = ConstT (mf *> ma)

{-# INLINE forT_ #-}
forT_ :: Applicative m => Traversal s t a b -> s -> (a -> m ()) -> m ()
forT_ t s f = unConstT $ t (ConstT . f) s

over :: Traversal s t a b -> (a -> b) -> (s -> t)
over t f = runIdentity . t (Identity . f)

instance Show Loc where
  showsPrec _ (Loc ln col) = shows ln . showChar ':' . shows col

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

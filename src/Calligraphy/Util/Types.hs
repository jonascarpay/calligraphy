{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Calligraphy.Util.Types where

import Data.Bitraversable (bitraverse)
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Forest)

-- | This is the main type that phases will operate on.
data Modules = Modules
  { _modules :: [(String, Forest Decl)],
    _calls :: Set (Key, Key),
    _inferences :: Set (Key, Key)
  }

data Decl = Decl
  { declName :: String,
    declKey :: Key,
    declExported :: Bool,
    declType :: DeclType,
    declLoc :: Loc
  }

-- TODO rename runKey
newtype Key = Key {runKey :: Int}
  deriving (Enum, Show, Eq, Ord)

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

instance Show Loc where
  showsPrec _ (Loc line col) = shows line . showChar ':' . shows col

-- TODO this is not a type
rekeyCalls :: (Enum a, Ord b) => EnumMap a b -> Set (a, a) -> Set (b, b)
rekeyCalls m = foldr (maybe id Set.insert . bitraverse (flip EnumMap.lookup m) (flip EnumMap.lookup m)) mempty

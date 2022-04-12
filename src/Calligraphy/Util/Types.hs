{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Calligraphy.Util.Types where

import Calligraphy.Util.Printer
import Control.Monad
import Data.Bitraversable (bitraverse)
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.Graph
import Data.Set (Set)
import qualified Data.Set as Set

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

newtype Key = Key {unKey :: Int}
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
  showsPrec _ (Loc ln col) = shows ln . showChar ':' . shows col

-- TODO this is not a type
rekeyCalls :: (Enum a, Ord b) => EnumMap a b -> Set (a, a) -> Set (b, b)
rekeyCalls m = foldr (maybe id Set.insert . bitraverse (flip EnumMap.lookup m) (flip EnumMap.lookup m)) mempty

ppModules :: Prints Modules
ppModules (Modules modules _ _) = forM_ modules $ \(modName, forest) -> do
  strLn modName
  indent $ mapM_ ppTree forest

ppTree :: Prints (Tree Decl)
ppTree (Node (Decl name _key _exp typ loc) children) = do
  strLn $ name <> " (" <> show typ <> ", " <> show loc <> ")"
  indent $ mapM_ ppTree children

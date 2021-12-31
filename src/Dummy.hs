{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Dummy
  ( ExportedT (Exported),
    exportedFun,
    Class (method),
    TypeSynonym,
  )
where

import Data.Functor.Identity
import Data.Kind (Type)
import Data.Map qualified as M

data ExportedT
  = Exported (Identity Int)
  | NotExported (M.Map Int Int)
  | Single

data Record
  = Record1 {field1 :: Int, field2 :: Record}
  | Record2 {field3 :: Int}
  | NoRecord Int

data LocalT
  = Loc1 Int Int
  | Loc2 Bool
  deriving (Show)

pattern Zero :: Int
pattern Zero = 0

data GADT a where
  GADTInt :: GADT Int
  GADTFloat :: GADT LocalT

class Class a where
  type CTF a :: Type
  data CDF a
  method :: a -> a
  default method :: a -> a
  method = id

  hiddenMethod :: a -> a

instance Class ExportedT where
  type CTF ExportedT = Int
  data CDF ExportedT = FooData
  method = id
  hiddenMethod = id

type TypeSynonym a = Int

type family TypeFamily a :: Type where
  TypeFamily Int = Float

newtype Newtype = Newtype {accessor :: Int}

exportedFun :: Int -> Int
exportedFun = a
  where
    a = b
    b = a

localFun :: Class a => a -> a
localFun = hiddenMethod

emptyMap :: M.Map Int Int
emptyMap = M.empty

expValue :: Int
expValue = 4

expArg :: a -> a
expArg x = x

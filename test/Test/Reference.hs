{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Reference
  ( ExportedT (Exported),
    exportedFun,
    Class (method),
    TypeSynonym,
  )
where

import Control.Monad hiding (when)
import Data.Functor.Identity
import Data.Kind (Type)
import qualified Data.Map as M

data ExportedT
  = Exported (Identity Int)
  | NotExported (M.Map Int Int)
  | Single

data Record
  = Record1 {field1 :: Int, field2 :: Record, field3 :: Int}
  | Record2 {field3 :: Int}
  | NoRecord Int

data LocalT
  = Loc1 Int Int
  | Loc2 Bool
  deriving (Show)

exportedFun :: Int -> Int
exportedFun = a
  where
    a = b
    b = a

pattern Zero :: Int
pattern Zero = 0

data WithSignature (a :: Type -> Type)

data GADT a where
  GADTInt :: GADT Int
  GADTFloat :: GADT LocalT

class Class a where
  type CTF a :: Type
  data CDF a
  method :: a -> LocalT
  default method :: a -> LocalT
  method _ = impl
    where
      impl = Loc2 True

  hiddenMethod :: a -> a

class Class a => SubClass a where
  soup :: a -> Int

instance Class ExportedT where
  type CTF ExportedT = Int
  data CDF ExportedT = FooData
  method _ = Loc1 1 2
  hiddenMethod = id

type TypeSynonym a = Int

type family TypeFamily a :: Type where
  TypeFamily Int = Float

newtype Newtype = Newtype {accessor :: Int}

localFun :: Class a => a -> a
localFun = hiddenMethod

untyped = Single

emptyMap :: M.Map Int Int
emptyMap = M.empty

recFn :: Record -> Record
recFn Record1 {field1 = x} = undefined x
recFn NoRecord {} = Record1 {field1 = 0, field2 = undefined}
recFn x = NoRecord $ field3 x

expValue :: Int -> Int
expValue 0 = 4
expValue x = error $ foo (Loc2 True)
  where
    foo :: LocalT -> String
    foo (Loc1 a b) = "asdfasdf"
    foo (Loc2 True) = "sdfgdfsg"
    foo (Loc2 False) = error "soup"

expArg :: a -> a
expArg x = x

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

import Data.Kind (Type)

{-
(DataDecl, TyClDecl)
  - ExportedT: Decl DataDec
  - (ConDeclH98, ConDecl)
    Exported: Decl ConDec
  - (ConDeclH98, ConDecl)
    NotExported: Decl ConDec
-}
data ExportedT
  = Exported
  | NotExported

{-
(DataDecl, TyClDecl)
  - Record: Decl DataDec
  - (ConDeclH98, ConDecl)
    - Record1: Decl ConDec
    -
      - ConDeclField, ConDeclField
        - (AbsBinds, HsBindLR) (FunBind, HsBindLR)
          field1: ValBind RegularBind, RecField RecFieldDecl
        - Int

-}
data Record
  = Record1 {field1 :: Int, field2 :: Record}
  | Record2 {field3 :: Int}

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

expValue :: Int
expValue = 4

expArg :: a -> a
expArg x = x

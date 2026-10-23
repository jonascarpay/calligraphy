{-# LANGUAGE CPP #-}

-- | Thin compatability layer that re-exports things from GHC.
module Calligraphy.Compat.GHC
  ( BindType (..),
    ContextInfo (..),
    DeclType (..),
    HieAST (..),
    HieASTs (..),
    HieFile (..),
    HieFileResult (..),
    HieType (..),
    HieTypeFlat,
    Identifier,
    IdentifierDetails (..),
    IfaceTyCon (..),
    ModuleName,
    Name,
    NameCache,
    NodeInfo (..),
    RealSrcLoc,
    RealSrcSpan,
    RecFieldContext (..),
    Scope (..),
    Span,
    TypeIndex,
    availNames,
    getKey,
    getOccString,
    hieVersion,
    initNameCache,
    mkSplitUniqSupply,
    moduleName,
    moduleNameString,
#if MIN_VERSION_ghc(9,14,0)
    newNameCache,
#endif
    nameUnique,
    realSrcSpanEnd,
    realSrcSpanStart,
    srcSpanStartCol,
    srcSpanStartLine,
    srcSpanEndCol,
    srcSpanEndLine,
    srcLocCol,
    srcLocLine,
  )
where

import Calligraphy.Prelude

#if MIN_VERSION_ghc(9,6,0)
import Language.Haskell.Syntax.Module.Name (moduleNameString, ModuleName)
#else
import GHC.Unit.Module.Name (moduleNameString, ModuleName)
#endif

import GHC.Iface.Type
import GHC.Types.Avail
import GHC.Types.Name
import GHC.Types.Unique
import GHC.Types.Unique.Supply
import GHC.Unit.Types

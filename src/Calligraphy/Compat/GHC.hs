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

#if MIN_VERSION_ghc(9,6,0)
import Language.Haskell.Syntax.Module.Name (moduleNameString, ModuleName)
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Unit.Module.Name (moduleNameString, ModuleName)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Iface.Type
import GHC.Types.Avail
import GHC.Types.Name
import GHC.Types.Unique
import GHC.Types.Unique.Supply
import GHC.Unit.Types
#else
import Avail
import GHC
import HieBin
import HieTypes
import IfaceType
import Name
import NameCache
import SrcLoc
import UniqSupply
import Unique
#endif

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Compat
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
    --
    forNodeInfos_,
    showContextInfo,
    readHieFileCompat,
    isInstanceNode,
    showAnns,
  )
where

import Control.Monad
import Data.IORef
import qualified Data.Set as Set
import Prelude

#if MIN_VERSION_ghc(9,0,0)
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Iface.Type
import GHC.Types.Avail
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Types.SrcLoc
import GHC.Types.SrcLoc
import GHC.Types.Unique
import GHC.Types.Unique.Supply
import GHC.Unit.Module.Name
import GHC.Unit.Types
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
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

forNodeInfos_ :: Monad m => HieAST a -> (NodeInfo a -> m ()) -> m ()
showContextInfo :: ContextInfo -> String
readHieFileCompat :: IORef NameCache -> FilePath -> IO HieFileResult
#if MIN_VERSION_ghc(9,0,0)

forNodeInfos_ (Node (SourcedNodeInfo sourcedNodeInfos) span children) = forM_ sourcedNodeInfos

showContextInfo = showSDocUnsafe . ppr

readHieFileCompat ref fp = readHieFile (NCU (atomicModifyIORef ref)) fp

#else

forNodeInfos_ (Node sourceInfo _ _) f = f sourceInfo

showContextInfo = show

readHieFileCompat ref fp = do
  cache <- readIORef ref
  (res, cache') <- readHieFile cache fp
  writeIORef ref cache'
  pure res

#endif

isInstanceNode :: NodeInfo a -> Bool
showAnns :: NodeInfo a -> String

#if MIN_VERSION_ghc(9,2,0)

isInstanceNode (NodeInfo anns _ _) = Set.member (NodeAnnotation "ClsInstD" "InstDecl") anns

showAnns (NodeInfo anns _ _) = unwords (show . unNodeAnnotation <$> Set.toList anns)
  where
    unNodeAnnotation (NodeAnnotation a b) = (a, b)

#else

isInstanceNode (NodeInfo anns _ _) = Set.member ("ClsInstD", "InstDecl") anns

showAnns (NodeInfo anns _ _) = unwords (show <$> Set.toList anns)

#endif

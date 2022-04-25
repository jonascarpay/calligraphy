{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-unused-matches #-}

module Calligraphy.Compat.Lib
  ( forNodeInfos_,
    showContextInfo,
    readHieFileCompat,
    isInstanceNode,
    isTypeSignatureNode,
    isInlineNode,
    isMinimalNode,
    isDerivingNode,
    showAnns,
    spanSpans,
  )
where

import Data.IORef
import qualified Data.Set as Set

#if MIN_VERSION_ghc(9,0,0)
import Control.Monad
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Types.Name.Cache
import GHC.Types.SrcLoc
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import qualified Data.Map as Map
#else
import HieBin
import HieTypes
import NameCache
import SrcLoc
#endif

-- TODO lensify
forNodeInfos_ :: Monad m => HieAST a -> (NodeInfo a -> m ()) -> m ()
showContextInfo :: ContextInfo -> String
readHieFileCompat :: IORef NameCache -> FilePath -> IO HieFileResult
#if MIN_VERSION_ghc(9,0,0)

forNodeInfos_ (Node (SourcedNodeInfo sourcedNodeInfos) _ _) = forM_ (Map.lookup SourceInfo sourcedNodeInfos)

showContextInfo = showSDocUnsafe . ppr

readHieFileCompat ref = readHieFile (NCU (atomicModifyIORef ref))

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
isTypeSignatureNode :: NodeInfo a -> Bool
isInlineNode :: NodeInfo a -> Bool
isMinimalNode :: NodeInfo a -> Bool
isDerivingNode :: NodeInfo a -> Bool
showAnns :: NodeInfo a -> String
#if MIN_VERSION_ghc(9,2,0)

isInstanceNode (NodeInfo anns _ _) = Set.member (NodeAnnotation "ClsInstD" "InstDecl") anns

isTypeSignatureNode (NodeInfo anns _ _) = Set.member (NodeAnnotation "TypeSig" "Sig") anns

isInlineNode (NodeInfo anns _ _) = Set.member (NodeAnnotation "InlineSig" "Sig") anns

isMinimalNode (NodeInfo anns _ _) = Set.member (NodeAnnotation "MinimalSig" "Sig") anns

isDerivingNode (NodeInfo anns _ _) = Set.member (NodeAnnotation "HsDerivingClause" "HsDerivingClause") anns

showAnns (NodeInfo anns _ _) = unwords (show . unNodeAnnotation <$> Set.toList anns)
  where
    unNodeAnnotation (NodeAnnotation a b) = (a, b)

#else

isInstanceNode (NodeInfo anns _ _) = Set.member ("ClsInstD", "InstDecl") anns

isTypeSignatureNode (NodeInfo anns _ _) = Set.member ("TypeSig", "Sig") anns

isInlineNode (NodeInfo anns _ _) = Set.member ("InlineSig", "Sig") anns

isMinimalNode (NodeInfo anns _ _) = Set.member ("MinimalSig", "Sig") anns

isDerivingNode (NodeInfo anns _ _) = Set.member ("HsDerivingClause", "HsDerivingClause") anns

showAnns (NodeInfo anns _ _) = unwords (show <$> Set.toList anns)

#endif

spanSpans :: Span -> Span -> Span
spanSpans sp1 sp2 =
  mkRealSrcSpan
    ( min
        (realSrcSpanStart sp1)
        (realSrcSpanStart sp2)
    )
    ( max
        (realSrcSpanEnd sp1)
        (realSrcSpanEnd sp2)
    )

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
    showAnns,
    classifyIdentifier,
  )
where

import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set

#if MIN_VERSION_ghc(9,0,0)
import Control.Monad
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Types.Name.Cache
import GHC.Types.SrcLoc
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
#else
import HieBin
import HieTypes
import NameCache
import SrcLoc
#endif

forNodeInfos_ :: Monad m => HieAST a -> (NodeInfo a -> m ()) -> m ()
showContextInfo :: ContextInfo -> String
readHieFileCompat :: IORef NameCache -> FilePath -> IO HieFileResult
#if MIN_VERSION_ghc(9,0,0)

forNodeInfos_ (Node (SourcedNodeInfo sourcedNodeInfos) _ _) = forM_ sourcedNodeInfos

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
showAnns :: NodeInfo a -> String
#if MIN_VERSION_ghc(9,2,0)

isInstanceNode (NodeInfo anns _ _) = Set.member (NodeAnnotation "ClsInstD" "InstDecl") anns

isTypeSignatureNode (NodeInfo anns _ _) = Set.member (NodeAnnotation "TypeSig" "Sig") anns

isInlineNode (NodeInfo anns _ _) = Set.member (NodeAnnotation "InlineSig" "Sig") anns

showAnns (NodeInfo anns _ _) = unwords (show . unNodeAnnotation <$> Set.toList anns)
  where
    unNodeAnnotation (NodeAnnotation a b) = (a, b)

#else

isInstanceNode (NodeInfo anns _ _) = Set.member ("ClsInstD", "InstDecl") anns

isTypeSignatureNode (NodeInfo anns _ _) = Set.member ("TypeSig", "Sig") anns

isInlineNode (NodeInfo anns _ _) = Set.member ("InlineSig", "Sig") anns

showAnns (NodeInfo anns _ _) = unwords (show <$> Set.toList anns)

#endif

classifyIdentifier ::
  Set ContextInfo ->
  (Span -> r) ->
  (Span -> r) ->
  (Span -> r) ->
  (Span -> r) ->
  (Span -> r) ->
  r ->
  r ->
  r ->
  r
classifyIdentifier ctx valdecl recdecl condecl datadecl classdecl use ignore unknown = case Set.toAscList ctx of
  [Decl DataDec (Just sp)] -> datadecl sp
  [Decl PatSynDec (Just sp)] -> datadecl sp
  [Decl FamDec (Just sp)] -> datadecl sp
  [Decl SynDec (Just sp)] -> datadecl sp
  [ClassTyDecl (Just sp)] -> valdecl sp
  [MatchBind, ValBind _ _ (Just sp)] -> valdecl sp
  [MatchBind] -> ignore
  [Decl InstDec _] -> ignore
  [Decl ConDec (Just sp)] -> condecl sp
  [Use] -> use
  [Use, RecField RecFieldOcc _] -> use
  [Decl ClassDec (Just sp)] -> classdecl sp
  [ValBind RegularBind ModuleScope (Just sp1), RecField RecFieldDecl (Just sp2)] -> recdecl (spanSpans sp1 sp2)
  -- -- Recordfields without valbind occur when a record occurs in multiple constructors
  [RecField RecFieldDecl (Just sp)] -> recdecl sp
  [RecField RecFieldAssign _] -> use
  [RecField RecFieldMatch _] -> ignore
  [RecField RecFieldOcc _] -> use
  [PatternBind _ _ _] -> ignore
  [TyDecl] -> ignore
  [IEThing _] -> ignore
  [TyVarBind _ _] -> ignore
  -- -- An empty ValBind is the result of a derived instance, and should be ignored
  [ValBind RegularBind ModuleScope _] -> ignore
#if MIN_VERSION_ghc(9,0,0)
  [MatchBind, RecField RecFieldMatch _] -> ignore
  [EvidenceVarBind _ _ _] -> ignore
  [EvidenceVarBind _ _ _, EvidenceVarUse] -> ignore
  [EvidenceVarUse] -> use
#endif
  _ -> unknown

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

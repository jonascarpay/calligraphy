{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-unused-matches #-}

module Calligraphy.Compat.Lib
  ( sourceInfo,
    showContextInfo,
    readHieFileCompat,
    isInstanceNode,
    isTypeSignatureNode,
    isInlineNode,
    isMinimalNode,
    isDerivingNode,
    showAnns,
    mergeSpans,
    isPointSpan,
    getHieFiles,
  )
where

import qualified Calligraphy.Compat.GHC as GHC
import Calligraphy.Prelude
import Calligraphy.Util.Lens
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set

#if MIN_VERSION_ghc(9,14,0)

getHieFiles :: [FilePath] -> IO [HieFile]
getHieFiles filePaths = do
  ref <- newIORef =<< GHC.newNameCache
  forM filePaths (readHieFileWithWarning ref)

#elif MIN_VERSION_ghc(9,4,0)

getHieFiles :: [FilePath] -> IO [HieFile]
getHieFiles filePaths = do
  ref <- newIORef =<< GHC.initNameCache 'z' []
  forM filePaths (readHieFileWithWarning ref)

#else

getHieFiles :: [FilePath] -> IO [HieFile]
getHieFiles filePaths = do
    uniqSupply <- GHC.mkSplitUniqSupply 'z'
    ref <- newIORef (GHC.initNameCache uniqSupply [])
    forM filePaths (readHieFileWithWarning ref)

#endif

readHieFileWithWarning :: IORef GHC.NameCache -> FilePath -> IO GHC.HieFile
readHieFileWithWarning ref path = do
  GHC.HieFileResult fileHieVersion fileGHCVersion hie <- readHieFileCompat ref path
  when (GHC.hieVersion /= fileHieVersion) $ do
    putStrLn $ "WARNING: version mismatch in " <> path
    putStrLn $ "    The hie files in this project were generated with GHC version: " <> show fileGHCVersion
    putStrLn $ "    This version of calligraphy was compiled with GHC version: " <> show GHC.hieVersion
    putStrLn "    Optimistically continuing anyway..."
  pure hie

{-# INLINE sourceInfo #-}
sourceInfo :: Traversal' (HieAST a) (NodeInfo a)
showContextInfo :: ContextInfo -> String
readHieFileCompat :: IORef NameCache -> FilePath -> IO HieFileResult

#if MIN_VERSION_ghc(9,4,0)

sourceInfo f (Node (SourcedNodeInfo inf) sp children) = (\inf' -> Node (SourcedNodeInfo inf') sp children) <$> Map.alterF (maybe (pure Nothing) (fmap Just . f)) SourceInfo inf

showContextInfo = showSDocUnsafe . ppr

readHieFileCompat ref path = do
  nameCache <- readIORef ref
  readHieFile nameCache path

#else

sourceInfo f (Node (SourcedNodeInfo inf) sp children) = (\inf' -> Node (SourcedNodeInfo inf') sp children) <$> Map.alterF (maybe (pure Nothing) (fmap Just . f)) SourceInfo inf

showContextInfo = showSDocUnsafe . ppr

readHieFileCompat ref = readHieFile (NCU (atomicModifyIORef ref))

#endif

isInstanceNode :: NodeInfo a -> Bool
isTypeSignatureNode :: NodeInfo a -> Bool
isInlineNode :: NodeInfo a -> Bool
isMinimalNode :: NodeInfo a -> Bool
isDerivingNode :: NodeInfo a -> Bool
showAnns :: NodeInfo a -> String

isInstanceNode (NodeInfo anns _ _) = any (flip Set.member anns) [NodeAnnotation "ClsInstD" "InstDecl", NodeAnnotation "DerivDecl" "DerivDecl"]

isTypeSignatureNode (NodeInfo anns _ _) = Set.member (NodeAnnotation "TypeSig" "Sig") anns

isInlineNode (NodeInfo anns _ _) = Set.member (NodeAnnotation "InlineSig" "Sig") anns

isMinimalNode (NodeInfo anns _ _) = Set.member (NodeAnnotation "MinimalSig" "Sig") anns

isDerivingNode (NodeInfo anns _ _) = Set.member (NodeAnnotation "HsDerivingClause" "HsDerivingClause") anns

showAnns (NodeInfo anns _ _) = unwords (show . unNodeAnnotation <$> Set.toList anns)
  where
    unNodeAnnotation (NodeAnnotation a b) = (a, b)

mergeSpans :: Span -> Span -> Span
mergeSpans sp1 sp2 =
  mkRealSrcSpan
    ( min
        (realSrcSpanStart sp1)
        (realSrcSpanStart sp2)
    )
    ( max
        (realSrcSpanEnd sp1)
        (realSrcSpanEnd sp2)
    )

isPointSpan :: Span -> Bool
isPointSpan sp = realSrcSpanEnd sp <= realSrcSpanStart sp

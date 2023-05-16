{-# LANGUAGE CPP #-}

-- | Debug tools for GHC-related data
module Calligraphy.Compat.Debug
  ( ppHieFile,
    ppIdentifier,
    showGHCName,
  )
where

import Calligraphy.Util.Printer
import qualified Data.Map as Map

#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Data.FastString as GHC
import qualified GHC.Iface.Ext.Types as GHC
import qualified GHC.Types.Name as GHC
import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Types.Unique as GHC
import qualified GHC.Unit as GHC
import qualified GHC.Utils.Outputable as GHC
#elif MIN_VERSION_ghc(9,0,0)
import qualified GHC.Data.FastString as GHC
import qualified GHC.Iface.Ext.Types as GHC
import qualified GHC.Types.Name as GHC
import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Types.Unique as GHC
import qualified GHC.Unit as GHC
import qualified GHC.Utils.Outputable as GHC
import qualified GHC.Driver.Session as GHC
#else
import qualified HieTypes as GHC
import qualified Module as GHC
import qualified FastString as GHC
import qualified GhcPlugins as GHC
import qualified Unique as GHC
#endif

ppHieFile :: Prints GHC.HieFile
ppHieFile (GHC.HieFile path (GHC.Module _ mdl) _types (GHC.HieASTs asts) _exps _src) = do
  strLn "Hie File"
  indent $ do
    strLn "path:"
    indent $ strLn path
    strLn "module: "
    indent $ strLn (GHC.moduleNameString mdl)
    strLn "contents:"
    indent $
#if MIN_VERSION_ghc(9,2,0)
      forM_ (Map.toList asts) $ \(GHC.LexicalFastString hiePath, ast) -> do
#else
      forM_ (Map.toList asts) $ \(hiePath, ast) -> do
#endif
        strLn (GHC.unpackFS hiePath)
        indent $ ppAst ast

ppAst :: GHC.HieAST a -> Printer ()
#if MIN_VERSION_ghc(9,2,0)
ppAst (GHC.Node (GHC.SourcedNodeInfo nodeInfo) spn children) = do
  strLn (showSpan spn)
  forM_ (Map.toList nodeInfo) $ \(origin, GHC.NodeInfo anns _ ids) -> do
    case origin of
      GeneratedInfo -> strLn "GeneratedInfo"
      SourceInfo -> strLn "SourceInfo"
    indent $  do
      forM_ (Map.toList ids) $ \(idn, GHC.IdentifierDetails _ idnDetails) -> do
        ppIdentifier idn
        indent $ forM_ idnDetails $ strLn . GHC.showSDocOneLine GHC.defaultSDocContext . GHC.ppr
      forM_ anns $ \(GHC.NodeAnnotation constr typ) -> strLn (show (constr, typ))
  indent $ mapM_ ppAst children
#elif MIN_VERSION_ghc(9,0,0)
ppAst (GHC.Node (GHC.SourcedNodeInfo nodeInfo) spn children) = do
  strLn (showSpan spn)
  forM_ nodeInfo $ \ (GHC.NodeInfo anns _ ids) -> do
    forM_ (Map.toList ids) $ \(idn, GHC.IdentifierDetails _ idnDetails) -> do
      ppIdentifier idn
      indent $ forM_ idnDetails $ strLn . GHC.showSDocOneLine (GHC.initSDocContext GHC.unsafeGlobalDynFlags GHC.defaultUserStyle) . GHC.ppr
    forM_ anns $ showLn
  indent $ mapM_ ppAst children
#else
ppAst (GHC.Node (GHC.NodeInfo anns _ ids) spn children) = do
  strLn (showSpan spn)
  forM_ (Map.toList ids) $ \(idn, GHC.IdentifierDetails _ idnDetails) -> do
    ppIdentifier idn
    indent $ forM_ idnDetails showLn
  mapM_ showLn anns
  indent $ mapM_ ppAst children
#endif

showSpan :: GHC.RealSrcSpan -> String
showSpan s =
  mconcat
    [ show $ GHC.srcSpanStartLine s,
      ":",
      show $ GHC.srcSpanStartCol s,
      " - ",
      show $ GHC.srcSpanEndLine s,
      ":",
      show $ GHC.srcSpanEndCol s
    ]

ppIdentifier :: Prints GHC.Identifier
ppIdentifier = strLn . either showModuleName showGHCName

showModuleName :: GHC.ModuleName -> String
showModuleName = flip mappend " (module)" . show . GHC.moduleNameString

showGHCName :: GHC.Name -> String
showGHCName name = GHC.getOccString name <> "    " <> show (GHC.getKey $ GHC.nameUnique name)

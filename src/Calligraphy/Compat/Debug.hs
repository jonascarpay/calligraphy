{-# LANGUAGE CPP #-}

module Calligraphy.Compat.Debug
  ( ppHieFile,
    ppIdentifier,
    showGHCName,
  )
where

import Calligraphy.Util.Printer
import Control.Monad
import qualified Data.Map as Map

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Data.FastString as GHC
import qualified GHC.Iface.Ext.Types as GHC
import qualified GHC.Types.Name as GHC
import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Types.Unique as GHC
import qualified GHC.Unit as GHC
import qualified GHC.Utils.Outputable as GHC
#else
import qualified HieTypes as GHC
import qualified Module as GHC
import qualified FastString as GHC
import qualified GhcPlugins as GHC
import qualified Unique as GHC
#endif

ppHieFile :: Prints GHC.HieFile
#if MIN_VERSION_ghc(9,0,0)
ppHieFile (GHC.HieFile path (GHC.Module _ mdl) _types (GHC.HieASTs asts) _exps _src) = do
  strLn "Hie File"
  indent $ do
    strLn "path:"
    indent $ strLn path
    strLn "module: "
    indent $ strLn (GHC.moduleNameString mdl)
    strLn "contents:"
    indent $
      forM_ (Map.toList asts) $ \(GHC.LexicalFastString hiePath, ast) -> do
        strLn (GHC.unpackFS hiePath)
        indent $ ppAst ast
  where
    ppAst :: GHC.HieAST a -> Printer ()
    ppAst (GHC.Node (GHC.SourcedNodeInfo nodeInfo) spn children) = do
      strLn (showSpan spn)
      forM_ nodeInfo $ \(GHC.NodeInfo anns _ ids) -> do
        forM_ (Map.toList ids) $ \(idn, GHC.IdentifierDetails _ idnDetails) -> do
          ppIdentifier idn
          indent $ forM_ idnDetails $ strLn . GHC.showSDocOneLine GHC.defaultSDocContext . GHC.ppr
        forM_ anns $ \(GHC.NodeAnnotation constr typ) -> strLn (show (constr, typ))
      indent $ mapM_ ppAst children
#else
ppHieFile (GHC.HieFile path (GHC.Module _ mdl) _types (GHC.HieASTs asts) _exps _src) = do
  strLn "Hie File"
  indent $ do
    strLn "path:"
    indent $ strLn path
    strLn "module: "
    indent $ strLn (GHC.moduleNameString mdl)
    strLn "contents:"
    indent $
      forM_ (Map.toList asts) $ \(hiePath, ast) -> do
        strLn (GHC.unpackFS hiePath)
        indent $ ppAst ast
  where
    ppAst :: GHC.HieAST a -> Printer ()
    ppAst (GHC.Node (GHC.NodeInfo anns _ ids) spn children) = do
      strLn (showSpan spn)
      forM_ (Map.toList ids) $ \(idn, GHC.IdentifierDetails _ idnDetails) -> do
        ppIdentifier idn
        indent $ forM_ idnDetails $ strLn . show
      forM_ anns $ strLn . show
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

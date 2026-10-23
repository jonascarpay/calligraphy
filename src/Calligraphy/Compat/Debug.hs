-- | Debug tools for GHC-related data
module Calligraphy.Compat.Debug
  ( ppHieFile,
    ppIdentifier,
    showGHCName,
  )
where

import Calligraphy.Prelude
import Calligraphy.Util.Printer
import qualified Data.Map as Map

import qualified GHC.Data.FastString as GHC
import qualified GHC.Iface.Ext.Types as GHC
import qualified GHC.Types.Name as GHC
import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Types.Unique as GHC
import qualified GHC.Unit as GHC
import qualified GHC.Utils.Outputable as GHC

ppHieFile :: Prints GHC.HieFile
ppHieFile GHC.HieFile{hie_hs_file=path, hie_module=GHC.Module _ mdl, hie_asts=GHC.HieASTs asts} = do
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

ppAst :: GHC.HieAST a -> Printer ()
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

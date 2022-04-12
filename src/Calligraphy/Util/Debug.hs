{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}

-- TODO export list

module Calligraphy.Util.Debug
  ( ppModules,
    ppHieFile,
    ppParseError,
    ppFilterError,
    ppModulesDebugInfo,
  )
where

import qualified Calligraphy.Compat.GHC as GHC
import Calligraphy.Compat.Lib (forNodeInfos_, showAnns, showContextInfo)
import Calligraphy.Phases.DependencyFilter (DependencyFilterError (..))
import Calligraphy.Phases.Parse
import Calligraphy.Util.LexTree (LexTree, TreeError (..), foldLexTree)
import Calligraphy.Util.Printer
import Calligraphy.Util.Types
import Control.Monad.RWS
import qualified Data.EnumSet as EnumSet
import qualified Data.Map as M
import Data.Tree

ppModules :: Prints Modules
ppModules (Modules modules _ _) = forM_ modules $ \(modName, forest) -> do
  strLn modName
  indent $ mapM_ ppTree forest

ppModulesDebugInfo :: Prints ModulesDebugInfo
ppModulesDebugInfo (ModulesDebugInfo mods) = forM_ mods $ \(modName, ltree) -> do
  strLn modName
  indent $ ppLexTree ltree

ppTree :: Prints (Tree Decl)
ppTree (Node (Decl name _key _exp typ loc) children) = do
  strLn $ name <> " (" <> show typ <> ", " <> show loc <> ")"
  indent $ mapM_ ppTree children

ppLexTree :: Prints (LexTree GHC.RealSrcLoc (DeclType, Name, Loc))
ppLexTree = foldLexTree (pure ()) $ \ls l (typ, name, _loc) m r rs -> do
  ls
  ppLocNode l r typ name
  indent m
  rs

-- TODO move to Parse
ppParseError :: Prints ParseError
ppParseError (UnhandledIdentifier nm sp inf) = do
  strLn $ "Unrecognized identifier: " <> showGHCName nm
  indent $ do
    strLn $ "loc: " <> show sp
    strLn $ "info:"
    indent $ mapM_ (strLn . showContextInfo) inf
ppParseError (TreeError err) = ppTreeError err

-- TODO move to Parse
ppFilterError :: Prints DependencyFilterError
ppFilterError (UnknownRootName root) = strLn $ "Unknown root name: " <> root

ppLocNode :: GHC.RealSrcLoc -> GHC.RealSrcLoc -> DeclType -> Name -> Printer ()
ppLocNode l r typ name = strLn $ showName name <> " (" <> show typ <> ") " <> show l <> " " <> show r

ppTreeError :: Prints (TreeError GHC.RealSrcLoc (DeclType, Name, Loc))
ppTreeError (InvalidBounds l (ty, nm, _) r) = strLn "Invalid bounds:" >> indent (ppLocNode l r ty nm)
ppTreeError (OverlappingBounds (ty, nm, _) (ty', nm', _) l r) = do
  strLn $ "OverlappingBounds bounds: (" <> show (l, r) <> ")"
  indent $ do
    strLn $ showName nm <> " (" <> show ty <> ")"
    strLn $ showName nm' <> " (" <> show ty' <> ")"
ppTreeError MidSplit = strLn "MidSplit"
ppTreeError (LexicalError l (ty, nm, _) r t) = do
  strLn "Lexical error"
  indent $ do
    ppLocNode l r ty nm
    ppLexTree t

ppHieFile :: Prints GHC.HieFile
ppHieFile (GHC.HieFile _ mdl _types (GHC.HieASTs asts) _exps _src) = do
  strLn $ showModuleName $ GHC.moduleName mdl
  indent $ forM_ asts ppNameTree
  where
    ppNameTree :: GHC.HieAST a -> Printer ()
    ppNameTree node@(GHC.Node _ spn children) =
      forNodeInfos_ node $ \nodeInfo -> do
        strLn $ ">> " <> showSpan spn <> " " <> showAnns nodeInfo
        indent $ do
          let pids = fmap GHC.identInfo <$> M.toList (GHC.nodeIdentifiers nodeInfo)
          forM_ pids $ \(idn, ctxInfo) -> do
            ppIdentifier idn
            indent $ mapM_ (strLn . showContextInfo) ctxInfo
          forM_ children ppNameTree

ppIdentifier :: Prints GHC.Identifier
ppIdentifier = strLn . either showModuleName showGHCName

showGHCName :: GHC.Name -> String
showGHCName name = GHC.getOccString name <> "    " <> show (GHC.getKey $ GHC.nameUnique name)

showName :: Name -> String
showName (Name name keys) = name <> "    " <> show (EnumSet.toList keys)

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

showModuleName :: GHC.ModuleName -> String
showModuleName = flip mappend " (module)" . show . GHC.moduleNameString

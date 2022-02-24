{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- TODO export list

module Debug
  ( ppHieFile,
    ppModule,
    ppParseError,
  )
where

import Control.Monad.RWS
import Data.Foldable
import Data.IntSet qualified as IntSet
import Data.Map qualified as M
import GHC qualified
import HieTypes qualified as GHC
import Name qualified as GHC
import Parse
import Printer
import STree (STree, TreeError (..))
import STree qualified
import SrcLoc
import Unique (getKey)

ppModule :: Prints Module
ppModule (Module modName _exports tree _calls) = do
  strLn modName
  ppTree tree

ppTree :: Prints (STree GHC.RealSrcLoc (DeclType, Name))
ppTree = STree.foldSTree (pure ()) $ \ls l (typ, name) m r rs -> do
  ls
  strLn $ showName name <> " (" <> show typ <> ") " <> show l <> " " <> show r
  indent m
  rs

ppParseError :: Prints ParseError
ppParseError (UnhandledIdentifier nm sp inf) = do
  strLn $ "Unrecognized identifier: " <> showGHCName nm
  indent $ do
    strLn $ "loc: " <> show sp
    strLn $ "info: " <> show inf
ppParseError (TreeError err) = ppTreeError err

ppNode :: GHC.RealSrcLoc -> GHC.RealSrcLoc -> DeclType -> Name -> Printer ()
ppNode l r typ name = strLn $ showName name <> " (" <> show typ <> ") " <> show l <> " " <> show r

ppTreeError :: Prints (TreeError GHC.RealSrcLoc (DeclType, Name))
ppTreeError (InvalidBounds l (ty, nm) r) = strLn "Invalid bounds:" >> indent (ppNode l r ty nm)
ppTreeError (OverlappingBounds (ty, nm) (ty', nm') l r) = do
  strLn $ "OverlappingBounds bounds: (" <> show (l, r) <> ")"
  indent $ do
    strLn $ showName nm <> " (" <> show ty <> ")"
    strLn $ showName nm' <> " (" <> show ty' <> ")"
ppTreeError MidSplit = strLn "MidSplit"
ppTreeError (LexicalError l (ty, nm) r t) = do
  strLn "Lexical error"
  indent $ do
    ppNode l r ty nm
    ppTree t

ppHieFile :: Prints GHC.HieFile
ppHieFile (GHC.HieFile _ mdl _types (GHC.HieASTs asts) _exps _src) = do
  strLn $ showModuleName $ GHC.moduleName mdl
  indent $ forM_ asts $ sequence_ . ppNameTree
  where
    ppNameTree :: GHC.HieAST a -> Maybe (Printer ())
    ppNameTree (GHC.Node (GHC.NodeInfo anns _ ids) spn children) =
      let subtrees = children >>= toList . ppNameTree
          pids = fmap GHC.identInfo <$> M.toList ids
       in if null subtrees && null pids && null anns
            then Nothing
            else pure $ do
              strLn $ ">> " <> showSpan spn <> " " <> unwords (fmap show (toList anns))
              indent $ do
                forM_ pids $ \(idn, ctxInfo) -> do
                  ppIdentifier idn
                  indent $ mapM_ (strLn . show) ctxInfo
                sequence_ subtrees

ppIdentifier :: Prints GHC.Identifier
ppIdentifier = strLn . either showModuleName showGHCName

showGHCName :: GHC.Name -> String
showGHCName name = GHC.getOccString name <> "    " <> show (getKey $ GHC.nameUnique name)

showName :: Name -> String
showName (Name name keys) = name <> "    " <> show (IntSet.toList keys)

showSpan :: RealSrcSpan -> String
showSpan s =
  mconcat
    [ show $ srcSpanStartLine s,
      ":",
      show $ srcSpanStartCol s,
      " - ",
      show $ srcSpanEndLine s,
      ":",
      show $ srcSpanEndCol s
    ]

showModuleName :: GHC.ModuleName -> String
showModuleName = flip mappend " (module)" . show . GHC.moduleNameString

{-
ppDeclTree :: Prints DeclTree
ppDeclTree (DeclTree typ (Name key name) _ chil) = do
  strLn $ name <> ": " <> show typ <> "   " <> show key
  indent $ mapM_ ppDeclTree (unScope chil)

ppFoldError :: Prints FoldError
ppFoldError StructuralError = strLn "Structural error"
ppFoldError (IdentifierError span err) = do
  strLn $ "Error constructing identifier at " <> showSpan span
  indent $ ppIdentifierError err

ppIdentifierError :: Prints IdentifierError
ppIdentifierError (UnhandledIdentifier idn info) = do
  strLn "Unhandled name"
  indent $ do
    strLn "Identifier"
    indent $ ppIdentifier idn
    strLn "Context"
    indent $ mapM_ (strLn . show) info

ppHieAst :: Prints (HieAST a)
ppHieAst (Node (NodeInfo anns _types ids) srcSpan children) = do
  strLn $ "Node " <> showSpan srcSpan
  indent $ do
    forM_ anns $ strLn . show
    forM_ (M.toList ids) $ \(idn, IdentifierDetails _type ctxInfo) -> do
      ppIdentifier idn
      indent $ mapM_ (strLn . show) ctxInfo
    mapM_ ppHieAst children

    -}

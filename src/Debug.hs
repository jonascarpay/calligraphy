{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- TODO export list

module Debug
  ( ppHieFile,
    ppParsedModule,
    ppModuleNameTree,
  )
where

import Control.Monad.RWS
import Data.Map qualified as M
import FastString qualified as GHC
import HieTypes hiding (nodeInfo)
import Module qualified as GHC
import Name
import Parse
import Printer
import SrcLoc

ppModuleNameTree :: Prints HieFile
ppModuleNameTree hief = do
  strLn $ showModuleName $ GHC.moduleName $ hie_module hief
  indent $ ppNameTree $ makeNameTree hief
  where
    ppNameTree :: Prints NameTree
    ppNameTree (NameTree ids chls spn) = do
      strLn $ ">>>> " <> showSpan spn
      indent . unless (null ids) $
        forM_ ids $ \(idn, ctxInfo) -> do
          strLn $ either showModuleName showName idn
          indent $ mapM_ (strLn . show) ctxInfo
      indent $ mapM_ ppNameTree chls

ppParsedModule :: Prints Module
ppParsedModule (Module name path decls imps) = do
  strLn $ name <> " " <> path
  indent . unless (null imps) $ do
    strLn "Imports"
    indent $ mapM_ strLn imps
  indent . unless (null decls) $ do
    strLn "Decls"
    indent $ mapM_ ppDecl decls

ppDecl :: Prints TopLevelDecl
ppDecl (TLData (DataType _ name cons)) = do
  strLn name
  indent . forM_ cons $ \(DataCon _ conName body) -> do
    strLn conName
    case body of
      DataConRecord fields -> indent $ forM_ fields $ \(_, field, _) -> strLn field
      _ -> pure ()
ppDecl (TLValue _) = undefined
ppDecl (TLClass _) = undefined

ppHieFile :: Prints HieFile
ppHieFile (HieFile path mdl _types (HieASTs asts) _exps _src) = do
  strLn path
  indent $ do
    strLn . showModuleName $ GHC.moduleName mdl
    forM_ (M.toList asts) $ \(fp, ast) -> do
      strLn $ GHC.unpackFS fp
      indent $ ppHieAst ast

ppHieAst :: Prints (HieAST a)
ppHieAst (Node (NodeInfo anns _types ids) srcSpan children) = do
  strLn $ "Node " <> showSpan srcSpan
  indent $ do
    forM_ anns $ strLn . show
    forM_ (M.toList ids) $ \(idn, IdentifierDetails _type ctxInfo) -> do
      strLn $ either showModuleName showName idn
      indent $ mapM_ (strLn . show) ctxInfo
    mapM_ ppHieAst children

showName :: Name -> String
showName = show . occNameString . nameOccName

showModuleName :: GHC.ModuleName -> String
showModuleName = flip mappend " (module)" . show . GHC.moduleNameString

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

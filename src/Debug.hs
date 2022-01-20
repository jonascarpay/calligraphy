{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- TODO export list

module Debug
  ( ppModuleNameTree,
    ppDeclTree,
    ppFoldError,
  )
where

import Control.Monad.RWS
import Data.Foldable
import Data.Map qualified as M
import HieTypes hiding (nodeInfo)
import HieTypes qualified as GHC
import Module qualified as GHC
import Name
import Parse
import Printer
import SrcLoc

ppDeclTree :: Prints DeclTree
ppDeclTree (DeclTree typ (Name _ name) _ chil) = do
  strLn $ name <> ": " <> show typ
  indent $ mapM_ ppDeclTree chil

ppFoldError :: Prints FoldError
ppFoldError StructuralError = strLn "Structural error"
ppFoldError (IdentifierError span err) = do
  strLn $ "Error constructing identifier at " <> showSpan span
  indent $ ppIdentifierError err
ppFoldError (NoFold heads) = do
  strLn "Error folding heads:"
  indent $ mapM_ ppFoldHead heads

ppFoldHead :: Prints FoldHead
ppFoldHead (FoldHead dep typ defs) = do
  strLn $ "Foldhead " <> show dep
  indent $
    forM_ defs $ \(name, use, chil) ->
      ppDeclTree $ DeclTree typ name use chil

ppIdentifierError :: Prints IdentifierError
ppIdentifierError (UnhandledIdentifier idn info) = do
  strLn "Unhandled name"
  indent $ do
    strLn "Identifier"
    indent $ ppIdentifier idn
    strLn "Context"
    indent $ mapM_ (strLn . show) info

ppModuleNameTree :: Prints HieFile
ppModuleNameTree (HieFile _ mdl _types (HieASTs asts) _exps _src) = do
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

ppIdentifier :: Prints Identifier
ppIdentifier = strLn . either showModuleName showName

ppHieAst :: Prints (HieAST a)
ppHieAst (Node (NodeInfo anns _types ids) srcSpan children) = do
  strLn $ "Node " <> showSpan srcSpan
  indent $ do
    forM_ anns $ strLn . show
    forM_ (M.toList ids) $ \(idn, IdentifierDetails _type ctxInfo) -> do
      ppIdentifier idn
      indent $ mapM_ (strLn . show) ctxInfo
    mapM_ ppHieAst children

showName :: Name.Name -> String
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

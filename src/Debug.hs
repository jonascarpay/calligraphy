{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- TODO export list

module Debug
  ( ppModuleNameTree,
    ppFoldNode,
    ppFoldError,
  )
where

import Control.Monad.RWS
import Data.Foldable
import Data.Map qualified as M
import FastString qualified as GHC
import HieTypes hiding (nodeInfo)
import HieTypes qualified as GHC
import Module qualified as GHC
import Name
import Parse
import Printer
import SrcLoc

ppFoldNode :: Prints FoldNode
ppFoldNode fn = strLn (show fn)

ppFoldError :: Prints FoldError
ppFoldError StructuralError = strLn "Structural error"
ppFoldError (AppendError span err) = do
  strLn $ "Error while combining at " <> showSpan span
  indent $ ppAppendError err
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

ppAppendError :: Prints AppendError
ppAppendError (CombineError anns l r) = do
  strLn "Could not combine"
  indent $ ppFoldNode l
  strLn "and"
  indent $ ppFoldNode r
  strLn "under"
  indent $ strLn $ unwords $ show <$> toList anns

ppValue :: Prints Value
ppValue (Value (Name _ str) children _) = strLn str >> indent (mapM_ ppValue children)

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

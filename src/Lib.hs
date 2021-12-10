{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Avail (AvailInfo (..))
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Map qualified as M
import Data.Set qualified as S
import Debug.Trace
import FieldLabel (flSelector)
import HieTypes hiding (nodeInfo)
import Lens.Micro.Platform
import Module
import Name
import Printer
import SrcLoc
import Unique

data Declaration = Declaration
  { declName :: String,
    declLoc :: RealSrcLoc,
    declScope :: Scope,
    declSubs :: IntSet,
    declCalls :: IntSet
  }

data ParsedModule = ParsedModule
  { pmName :: String,
    pmExports :: IntSet,
    pmBinds :: IntSet
  }

renderDecls :: IntMap Declaration -> Int -> Printer ()
renderDecls decls i =
  case IM.lookup i decls of
    Just (Declaration name loc _scope subs _) -> do
      strLn $ name <> "\t\t\t\t" <> show loc
      indent $ forM_ (IS.toList subs) (renderDecls decls) -- TODO worker wrapper
    Nothing -> error "impossible"

topLevel :: Prints HieFile
topLevel (HieFile _path modName _types (HieASTs asts) _exports _src) =
  let ((subs, _uses), decls) = runState (execWriterT $ mapM_ findDecl asts) mempty
   in do
        pModuleName $ moduleName modName
        indent $ forM_ (IS.toList subs) (renderDecls decls)

nameKey :: Name -> Int
nameKey = getKey . nameUnique

parseModule :: HieFile -> State (IntMap Declaration) ParsedModule
parseModule (HieFile _path (Module _ modName) _types (HieASTs asts) exps _src) = do
  let exports =
        IS.fromList $
          exps >>= \case
            Avail name -> [nameKey name]
            AvailTC name names fields -> fmap nameKey $ name : (names <> (flSelector <$> fields))
  (decls, _) <- execWriterT $ traverse findDecl asts
  pure $ ParsedModule (moduleNameString modName) exports decls

findDecl :: HieAST a -> WriterT (IntSet, IntSet) (State (IntMap Declaration)) ()
findDecl (Node (NodeInfo _anns _types idents) _span children) =
  case children >>= extractBinders of
    [(loc, name, scope)] -> do
      (subs, uses) <- lift . execWriterT $ forM children findDecl
      let uid = nameKey name
      tell (IS.singleton uid, mempty)
      at uid ?= Declaration (occNameString $ nameOccName name) (realSrcSpanStart loc) scope subs uses
    [] -> do
      forM_ (extractUse idents) $ \name -> tell (mempty, IS.singleton $ nameKey name)
      forM_ children findDecl
    ls -> trace (unlines $ fmap (\(spn, nm, _) -> show $ (spn, occNameString $ nameOccName nm)) ls) undefined

extractBinders :: HieAST a -> [(Span, Name, Scope)]
extractBinders (Node (NodeInfo _anns _types idents) span _children) =
  M.toList idents >>= \case
    (Right name, IdentifierDetails _ ctx) -> do
      scope <- S.toList ctx >>= isDecl
      pure (span, name, scope)
    _ -> []
  where
    isDecl :: ContextInfo -> [Scope]
    isDecl (ValBind _ scope _) = [scope]
    isDecl (Decl ConDec _) = [ModuleScope]
    isDecl _ = []

extractUse :: NodeIdentifiers a -> [Name]
extractUse idents =
  M.toList idents >>= \case
    (Right name, IdentifierDetails _ ctx) | S.member Use ctx -> pure name
    _ -> []

hieFile :: Prints HieFile
hieFile (HieFile path mod _types (HieASTs asts) _exps _src) = do
  strLn path
  indent $ do
    textLn "Module name"
    indent $ pModuleName $ moduleName mod
    textLn "Children"
    forM_ asts $ indent . hieAst

hieAst :: Prints (HieAST a)
hieAst (Node (NodeInfo anns _types ids) span children) = do
  strLn $ "Node " <> show span
  indent $ do
    textLn "Annotations"
    indent $ forM_ anns $ strLn . show
    textLn "Identifiers"
    indent $
      forM_ (M.toList ids) $ \(idn, IdentifierDetails _type ctxInfo) -> do
        pIdentifier idn
        indent $ mapM_ pContextInfo ctxInfo
    textLn "Children"
    mapM_ (indent . hieAst) children

pContextInfo :: Prints ContextInfo
pContextInfo = strLn . show

pIdentifier :: Prints Identifier
pIdentifier = either pModuleName pName

pName :: Name -> Printer ()
-- pName = strLn . show . nameUnique
pName = strLn . occNameString . nameOccName

pModuleName :: ModuleName -> Printer ()
pModuleName = strLn . moduleNameString

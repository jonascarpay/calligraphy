{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- TODO export list

module Lib where

import Avail (AvailInfo (..))
import Control.Monad.RWS
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Debug.Trace
import FieldLabel (flSelector)
import HieTypes hiding (nodeInfo)
import Module
import Name
import Printer
import SrcLoc
import Unique

-- TODO just take a Name instead of String and Key
data Declaration = Declaration
  { declName :: String,
    declKey :: Int,
    declLoc :: RealSrcLoc,
    declScope :: Scope,
    declSubs :: [Declaration],
    declCalls :: IntSet
  }

data ParsedModule = ParsedModule
  { pmName :: String,
    pmExports :: IntSet,
    pmBinds :: [Declaration]
  }

renderDecls :: Declaration -> Printer ()
renderDecls decl = go decl
  where
    go (Declaration name key loc _scope subs calls) = do
      strLn $ name <> "\t\t" <> show key <> "\t\t" <> show loc
      indent $ do
        strLn "calls:"
        indent $
          forM_ (IS.toList calls) $ \call -> strLn $ fromMaybe ("unknown " <> show call) (names M.!? call)
        strLn "children:"
        indent $ forM_ subs go -- TODO worker wrapper
    names :: Map Int String
    names = go decl
      where
        go (Declaration name key _ _ subs _) = M.singleton key (name <> " " <> show key) <> foldMap go subs

topLevel :: Prints HieFile
topLevel (HieFile _path modName _types (HieASTs asts) _exports _src) =
  let (_calls, decls) = foldMap findDecl asts
   in do
        pModuleName $ moduleName modName
        indent $ forM_ decls renderDecls

nameKey :: Name -> Int
nameKey = getKey . nameUnique

parseModule :: HieFile -> ParsedModule
parseModule (HieFile _path (Module _ modName) _types (HieASTs asts) exps _src) = ParsedModule (moduleNameString modName) exports decls
  where
    exports =
      IS.fromList $
        exps >>= \case
          Avail name -> [nameKey name]
          AvailTC name names fields -> fmap nameKey $ name : (names <> (flSelector <$> fields))
    (_, decls) = foldMap findDecl asts

-- TODO findDecl is essentially a naive writer, maybe CPS it

-- | A node is a declaration if any of its chilrdren are binders.
findDecl :: HieAST a -> (IntSet, [Declaration])
findDecl (Node (NodeInfo _anns _types idents) _span children) =
  case children >>= extractBinders of
    [] ->
      let calls = M.toList idents >>= uncurry extractUse
       in (IS.fromList (nameKey <$> calls) <> subCalls, subDecls)
    [(loc, name, scope)] ->
      (mempty, [Declaration (nameString name) (nameKey name) (realSrcSpanStart loc) scope subDecls subCalls])
    ls ->
      (mempty, flip fmap ls $ \(span, name, scope) -> traceShow ("subping", nameString name) $ Declaration (nameString name) (nameKey name) (realSrcSpanStart span) scope mempty mempty)
    _ -> error "multiple declarations, but also child calls"
  where
    (subCalls, subDecls) = foldMap findDecl children

nameString :: Name -> String
nameString = occNameString . nameOccName

-- ls -> forM_ ls $ \(loc, name, scope) -> do
-- (subs, uses) <- lift . execWriterT $ forM children findDecl
-- let uid = nameKey name
-- tell (IS.singleton uid, mempty)
-- at uid ?= Declaration (occNameString $ nameOccName name) (realSrcSpanStart loc) scope (IS.toList subs) uses

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

extractUse :: Identifier -> IdentifierDetails a -> [Name]
extractUse (Right name) (IdentifierDetails _ ctx) | S.member Use ctx = [name]
extractUse _ _ = []

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

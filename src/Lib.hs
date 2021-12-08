{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import HieTypes hiding (nodeInfo)
import Lens.Micro.Platform
import Module
import Name
import SrcLoc
import Unique

data Declaration = Declaration
  { declName :: String,
    declLoc :: RealSrcLoc,
    declScope :: Scope,
    declSub :: IntSet,
    declUse :: IntSet
  }

renderDecls :: IntMap Declaration -> Int -> Printer ()
renderDecls decls i =
  case IM.lookup i decls of
    Just (Declaration name span scope subs _) -> do
      strLn $ name <> "\t\t\t\t" <> show span
      indent $ forM_ (IS.toList subs) (renderDecls decls) -- TODO worker wrapper
    Nothing -> error "impossible"

topLevel :: Prints HieFile
topLevel (HieFile _path modName _types (HieASTs asts) _exports _src) =
  let ((subs, _uses), decls) = runState (execWriterT $ mapM_ findDecl asts) mempty
   in do
        pModuleName $ moduleName modName
        indent $ forM_ (IS.toList subs) (renderDecls decls)

findDecl :: HieAST a -> WriterT (IntSet, IntSet) (State (IntMap Declaration)) ()
findDecl (Node (NodeInfo _anns _types idents) _span children) =
  case children >>= extractBinders of
    [(span, name, scope)] -> do
      (subs, uses) <- lift . execWriterT $ forM children findDecl
      let uid = getKey . nameUnique $ name
      tell (IS.singleton uid, mempty)
      at uid ?= Declaration (occNameString $ nameOccName name) (realSrcSpanStart span) scope subs uses
    [] -> do
      forM_ (extractUse idents) $ \name -> tell (mempty, IS.singleton $ getKey . nameUnique $ name)
      forM_ children findDecl
    _ -> error "wat"

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

type Printer = RWS Int () Builder

type Prints a = a -> Printer ()

runPrinter :: Printer () -> Text
runPrinter p = TL.toStrict . B.toLazyText . fst $ execRWS p 0 mempty

indent :: Printer a -> Printer a
indent = local (+ 4)

line :: Prints Builder
line t = do
  n <- ask
  modify $
    flip mappend $ fold (replicate n (B.singleton ' ')) <> t <> B.singleton '\n'

strLn :: Prints String
strLn = line . B.fromString

textLn :: Prints Text
textLn = line . B.fromText

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

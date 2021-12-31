{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parse where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Either
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import FastString
import GHC qualified
import GHC.Arr (Array)
import GHC.Arr qualified as Array
import HieTypes qualified as GHC
import Name qualified as GHC
import Unique qualified as GHC

-- * TreeParser

type TreeParser n = ReaderT n Maybe

zoom :: (n -> n') -> TreeParser n' a -> TreeParser n a
zoom = withReaderT

pAll :: TreeParser n a -> TreeParser [n] [a]
pAll (ReaderT sub) = ReaderT $ traverse sub

pMany :: TreeParser n a -> TreeParser [n] [a]
pMany (ReaderT sub) = ReaderT $ \ns -> pure $ ns >>= toList . sub

pAny :: TreeParser n a -> TreeParser [n] a
pAny (ReaderT sub) = ReaderT $ asum . fmap sub

pSome :: TreeParser n a -> TreeParser [n] (NonEmpty a)
pSome sub =
  pMany sub >>= \case
    [] -> empty
    (h : t) -> pure $ h :| t

pOne :: TreeParser n a -> TreeParser [n] a
pOne sub =
  pMany sub >>= \case
    [a] -> pure a
    _ -> empty

pSearchDfs :: (n -> [n]) -> TreeParser n a -> TreeParser n a
pSearchDfs f sub = go
  where
    go = sub <|> zoom f (pAny go)

liftP2 :: (a -> b -> c) -> TreeParser n a -> TreeParser [n] b -> TreeParser [n] c
liftP2 f (ReaderT pa) (ReaderT pb) = ReaderT $ \case
  [] -> empty
  (h : t) -> liftA2 f (pa h) (pb t)

pGuard :: (n -> Bool) -> TreeParser n ()
pGuard f = asks f >>= guard

-- * Resolve

-- TODO this can be much more efficient. Maybe do something with TangleT?
resolve :: Array GHC.TypeIndex GHC.HieTypeFlat -> Array GHC.TypeIndex [Key]
resolve arr = imap (\i -> evalState (resolve1 i) mempty) arr
  where
    imap :: (GHC.TypeIndex -> b) -> Array GHC.TypeIndex a -> Array GHC.TypeIndex b
    imap f aa = Array.array (Array.bounds aa) ((\i -> (i, f i)) <$> Array.indices aa)
    resolve1 :: GHC.TypeIndex -> State (Set GHC.TypeIndex) [Key]
    resolve1 current = do
      gets (Set.member current) >>= \case
        True -> pure []
        False -> do
          modify (Set.insert current)
          case arr Array.! current of
            GHC.HTyVarTy name -> pure [nameKey name]
            a -> fold <$> traverse resolve1 a

-- * AstParser

type AstParser = TreeParser (GHC.HieAST [Key])

annotation :: (FastString, FastString) -> AstParser ()
annotation ann = pGuard (Set.member ann . GHC.nodeAnnotations . GHC.nodeInfo)

noAnnotation :: AstParser ()
noAnnotation = pGuard (null . GHC.nodeAnnotations . GHC.nodeInfo)

newtype Key = Key Int

-- * NameTree

type Identifier = (GHC.Identifier, Set GHC.ContextInfo)

data NameTree = NameTree
  { ntNames :: [Identifier],
    ntChildren :: [NameTree],
    ntSpan :: GHC.Span
  }

makeNameTree :: GHC.HieFile -> NameTree
makeNameTree (GHC.HieFile _ _ _ (GHC.HieASTs asts) _ _) =
  case go <$> toList asts of
    [tree] -> tree
    -- [([], tree)] -> tree
    _ -> error "huhu"
  where
    isEmpty :: NameTree -> Bool
    isEmpty (NameTree [] [] _) = True
    isEmpty _ = False

    go :: GHC.HieAST a -> NameTree
    go (GHC.Node (GHC.NodeInfo _anns _types ids) span children) =
      NameTree
        (fmap GHC.identInfo <$> M.toList ids)
        (filter (not . isEmpty) $ go <$> children)
        span

-- * Module

parseHieFile :: GHC.HieFile -> Maybe Module
parseHieFile = runReaderT pModule

data Module = Module
  { mdName :: String,
    mdPath :: FilePath,
    mdDecls :: [TopLevelDecl],
    mdImports :: [String]
  }

pModule :: TreeParser GHC.HieFile Module
pModule = do
  mdName <- asks (GHC.moduleNameString . GHC.moduleName . GHC.hie_module)
  typeArray <- asks (resolve . GHC.hie_types)
  mdPath <- asks GHC.hie_hs_file
  (mdImports, mdDecls) <- pOne ((fmap . fmap) (typeArray Array.!) . M.elems . GHC.getAsts . GHC.hie_asts) $ do
    annotation ("Module", "Module")
    fmap partitionEithers $
      pMany GHC.nodeChildren $
        Left <$> pImport <|> Right <$> pTopLevelDecl
  pure $ Module {..}

pImport :: AstParser String
pImport = do
  pOne GHC.nodeChildren $ do
    pOne (M.toList . GHC.nodeIdentifiers . GHC.nodeInfo) $
      ask >>= \case
        (Left mname, GHC.IdentifierDetails _ ctx)
          | Set.member (GHC.IEThing GHC.Import) ctx -> pure $ GHC.moduleNameString mname
        _ -> empty

data TopLevelDecl
  = TLData DataType
  | TLValue Value
  | TLClass Class

pTopLevelDecl :: AstParser TopLevelDecl
pTopLevelDecl =
  asum
    [ TLData <$> pData,
      TLValue <$> pValue,
      TLClass <$> pClass
    ]

-- * Data types

names :: GHC.HieAST a -> [GHC.Name]
names = (>>= toList) . M.keys . GHC.nodeIdentifiers . GHC.nodeInfo

unname :: GHC.Name -> (Key, String)
unname n = (nameKey n, GHC.occNameString $ GHC.nameOccName n)

nameKey :: GHC.Name -> Key
nameKey = Key . GHC.getKey . GHC.nameUnique

data DataType = DataType
  { dtKey :: Key,
    dtName :: String,
    dtCons :: [DataCon]
  }

data DataCon = DataCon
  { dcKey :: Key,
    dcName :: String,
    dcBody :: DataConBody
  }

data DataConBody
  = DataConRecord [(Key, String, [Key])]
  | DataConNaked [Key]

pName :: (Set GHC.ContextInfo -> Bool) -> AstParser (Key, String)
pName fCtx =
  pOne (M.toList . GHC.nodeIdentifiers . GHC.nodeInfo) $
    ask >>= \case
      (Right name, GHC.IdentifierDetails _ info) | fCtx info -> pure $ unname name
      _ -> empty

pUniqueNameChild :: (GHC.ContextInfo -> Bool) -> AstParser (Key, String)
pUniqueNameChild f = pOne GHC.nodeChildren $ pName (any f)

pData :: AstParser DataType
pData = do
  (dtKey, dtName) <-
    pUniqueNameChild $ \case
      GHC.Decl GHC.DataDec _ -> True
      _ -> False

  dtCons <- pMany GHC.nodeChildren $ do
    (dcKey, dcName) <- pUniqueNameChild $ \case
      GHC.Decl GHC.ConDec _ -> True
      _ -> False
    dcBody <-
      asum
        [ pOne GHC.nodeChildren $
            fmap (DataConRecord . toList) $
              pSome GHC.nodeChildren $ do
                (k, s) <- pUniqueNameChild $ \case
                  GHC.RecField GHC.RecFieldDecl _ -> True
                  _ -> False
                uses <- pUses
                pure (k, s, uses),
          DataConNaked <$> pUses
        ]
    pure $ DataCon {..}

  pure $ DataType {..}

pUses :: AstParser [Key]
pUses = asks astKeys
  where
    astKeys :: GHC.HieAST [Key] -> [Key]
    astKeys (GHC.Node (GHC.NodeInfo _ types ids) _ has) = concat types <> foldMap (uncurry idKeys) (Map.toList ids) <> foldMap astKeys has
    idKeys :: GHC.Identifier -> GHC.IdentifierDetails [Key] -> [Key]
    idKeys (Right name) (GHC.IdentifierDetails mkey ctxInfo) | Set.member GHC.Use ctxInfo = nameKey name : concat (toList mkey)
    idKeys _ _ = []

data Value

data Class = Class
  { clKey :: Key,
    clName :: String,
    clMethods :: [ClassMethod]
  }

data ClassMethod = ClassMethod

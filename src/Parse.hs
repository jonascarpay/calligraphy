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
import Data.Map (Map)
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

clocal :: r' -> TreeParser r' a -> TreeParser r a
clocal r' = withReaderT (const r')

pAll :: (n -> [n']) -> TreeParser n' a -> TreeParser n [a]
pAll f sub = asks f >>= mapM (flip clocal sub)

pMany :: (n -> [n']) -> TreeParser n' a -> TreeParser n [a]
pMany f sub = (>>= toList) <$> pAll f (optional sub)

pSome :: (n -> [n']) -> TreeParser n' a -> TreeParser n (NonEmpty a)
pSome f sub =
  pMany f sub >>= \case
    [] -> empty
    (h : t) -> pure $ h :| t

pAny :: (n -> [n']) -> TreeParser n' a -> TreeParser n a
pAny f sub = asks f >>= go
  where
    go [] = empty
    go (h : t) = clocal h sub <|> go t

pOne :: (n -> [n']) -> TreeParser n' a -> TreeParser n a
pOne f sub =
  pMany f sub >>= \case
    [a] -> pure a
    _ -> empty

pCheck :: (n -> Bool) -> TreeParser n ()
pCheck f = asks f >>= guard

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
annotation ann = pCheck (Set.member ann . GHC.nodeAnnotations . GHC.nodeInfo)

noAnnotation :: AstParser ()
noAnnotation = pCheck (null . GHC.nodeAnnotations . GHC.nodeInfo)

newtype Key = Key Int

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
  annotation ("ImportDecl", "ImportDecl")
  pOne GHC.nodeChildren $
    pOne (M.keys . GHC.nodeIdentifiers . GHC.nodeInfo) $
      ask >>= either (pure . GHC.moduleNameString) (const empty)

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
    dcFields :: Either [Key] [(String, [Key])]
  }

pData :: AstParser DataType
pData = do
  annotation ("DataDecl", "TyClDecl")
  (dtKey, dtName) <-
    pOne GHC.nodeChildren $ do
      noAnnotation
      pOne names $ asks unname

  dtCons <- pMany GHC.nodeChildren $ do
    annotation ("ConDeclH98", "ConDecl")
    (dcKey, dcName) <- pOne names $ asks unname
    dcDeps <- pUses
    pure $ DataCon {..}

  pure $ DataType {..}

pDataRecordField :: AstParser (String, Key, [Key])
pDataRecordField = do
  annotation ("ConDeclField", "ConDeclField")
  (k, s) <- pOne GHC.nodeChildren $ do
    annotation ("AbsBinds", "HsBindLR")
    annotation ("FunBind", "HsBindLR")
    pOne names $ asks unname
  deps <- pUses
  pure (s, k, deps)

pUses :: AstParser [Key]
pUses = error "not implasdf"
  where
    isUse :: GHC.IdentifierDetails a -> Bool
    isUse (GHC.IdentifierDetails _ s) = Set.member GHC.Use s
    pUse :: AstParser Key
    pUse = undefined

pClass :: AstParser Class
pClass = empty --error "not implemented"

pValue :: AstParser Value
pValue = empty --error "not implemented"

data Value

-- (ClassDecl, TyClDecl)
data Class = Class
  { clKey :: Key,
    clName :: String,
    clMethods :: [ClassMethod]
  }

data ClassMethod = ClassMethod

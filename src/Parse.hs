{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parse where

import Control.Applicative
import Control.Monad.Reader
import Data.Either
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as M
import Data.Set qualified as Set
import FastString
import GHC qualified
import HieTypes

-- * TreeParser

type TreeParser n = ReaderT n Maybe

clocal :: r' -> TreeParser r' a -> TreeParser r a
clocal r' = withReaderT (const r')

pAll, pMany :: (n -> [n']) -> TreeParser n' a -> TreeParser n [a]
pAll f sub = asks f >>= mapM (flip clocal sub)
pMany f sub = (>>= toList) <$> pAll f (optional sub)

pSome :: (n -> [n']) -> TreeParser n' a -> TreeParser n (NonEmpty a)
pSome f sub =
  pMany f sub >>= \case
    [] -> empty
    (h : t) -> pure $ h :| t

pAny, pOne :: (n -> [n']) -> TreeParser n' a -> TreeParser n a
pAny f sub = asks f >>= go
  where
    go [] = empty
    go (h : t) = clocal h sub <|> go t
pOne f sub =
  pMany f sub >>= \case
    [a] -> pure a
    _ -> empty

pCheck :: (n -> Bool) -> TreeParser n ()
pCheck f = asks f >>= guard

-- * AstParser

type AstParser = TreeParser (HieAST TypeIndex)

annotation :: (FastString, FastString) -> AstParser ()
annotation ann = pCheck (Set.member ann . nodeAnnotations . nodeInfo)

type Key = Int

-- * Module

parseHieFile :: HieFile -> Maybe Module
parseHieFile = runReaderT pModule

data Module = Module
  { mdName :: String,
    mdPath :: FilePath,
    mdDecls :: [TopLevelDecl],
    mdImports :: [String]
  }

pModule :: TreeParser HieFile Module
pModule = do
  mdName <- asks (GHC.moduleNameString . GHC.moduleName . hie_module)
  mdPath <- asks hie_hs_file
  (mdImports, mdDecls) <- pOne (M.elems . getAsts . hie_asts) $ do
    annotation ("Module", "Module")
    fmap partitionEithers $
      pMany nodeChildren $
        Left <$> pImport <|> Right <$> pTopLevelDecl
  pure $ Module {..}

pTopLevelDecl :: AstParser TopLevelDecl
pTopLevelDecl = empty -- error "not implemented"

pImport :: AstParser String
pImport = do
  annotation ("ImportDecl", "ImportDecl")
  pOne nodeChildren $
    pOne (M.keys . nodeIdentifiers . nodeInfo) $
      ask >>= either (pure . GHC.moduleNameString) (const empty)

data TopLevelDecl
  = TLData DataType
  | TLClass Class

-- DataDecl, TyClDecl
data DataType = DataType
  { dtKey :: Int,
    dtName :: String,
    dtCons :: [DataCon]
  }

-- (_, ConDecl)
data DataCon = DataCon
  { dcKey :: Int,
    dcName :: String
  }

-- (ClassDecl, TyClDecl)
data Class = Class
  { clKey :: Int,
    clName :: Int,
    clMethods :: [ClassMethod]
  }

data ClassMethod = ClassMethod

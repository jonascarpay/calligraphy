{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad.RWS
import Data.Foldable
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import HieTypes hiding (nodeInfo)
import Module
import Name

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
hieFile hf = do
  strLn $ hie_hs_file hf
  indent $ do
    textLn "Module name"
    indent $ pModuleName $ moduleName $ hie_module hf
    textLn "Children"
    forM_ (getAsts $ hie_asts hf) $ indent . hieAst

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
pName = strLn . show . nameUnique

pModuleName :: ModuleName -> Printer ()
pModuleName = strLn . moduleNameString

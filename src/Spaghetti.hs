{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides an entry point to the Weeder executable.
module Spaghetti (main, mainWithConfig) where

import Config
import Control.Monad.RWS
import Control.Monad.State
import Data.List (isPrefixOf)
import Data.Text.IO qualified as T
import Data.Version (showVersion)
import GraphViz
import HieBin
import HieTypes
import Lib
import NameCache
import Options.Applicative
import Paths_spaghetti (version)
import Printer
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath (isExtensionOf, replaceExtension, (</>))
import UniqSupply

main :: IO ()
main = do
  config <- execParser $ info (pConfig <**> helper <**> versionP) mempty
  mainWithConfig config
  where
    versionP =
      infoOption
        ( "spaghetti version "
            <> showVersion version
            <> "\nhie version "
            <> show hieVersion
        )
        (long "version" <> help "Show version")

mainWithConfig :: AppConfig -> IO ()
mainWithConfig (AppConfig searchConfig renderConfig outputConfig) = do
  hieFilePaths <- searchFiles searchConfig
  print hieFilePaths

  nameCache <- do
    uniqSupply <- mkSplitUniqSupply 'z'
    return (initNameCache uniqSupply [])

  hieFiles <-
    flip evalStateT nameCache $
      forM hieFilePaths readHieFileWithWarning

  let (mods, env) = flip runState mempty $ mapM parseModule hieFiles
      txt = runPrinter $ render env renderConfig mods

  case outputConfig of
    OutputStdOut -> T.putStr txt
    OutputFile fp -> T.writeFile fp txt

searchFiles :: SearchConfig -> IO [FilePath]
searchFiles SearchConfig {searchDotPaths, searchRoots} = do
  roots <- mapM makeAbsolute (if null searchRoots then ["./."] else searchRoots)
  foldMap go roots
  where
    go :: FilePath -> IO [FilePath]
    go path = do
      isFile <- doesFileExist path
      if isFile && isExtensionOf ".hie" path
        then pure [path]
        else do
          isDir <- doesDirectoryExist path
          if isDir
            then do
              contents <- (if searchDotPaths then id else filter (not . isPrefixOf ".")) <$> listDirectory path
              foldMap (go . (path </>)) contents
            else pure []

readHieFileWithWarning :: FilePath -> StateT NameCache IO HieFile
readHieFileWithWarning path = do
  nameCache <- get
  (HieFileResult fileHieVersion fileGHCVersion hie, cache') <- liftIO $ readHieFile nameCache path
  when (hieVersion /= fileHieVersion) $
    liftIO $ do
      putStrLn $ "WARNING: version mismatch in " <> path
      putStrLn $ "    The hie files in this project were generated with GHC version: " <> show fileGHCVersion
      putStrLn $ "    This version of spaghetti was compiled with GHC version: " <> show hieVersion
      putStrLn $ "    Optimistically continuing anyway..."
  hie <$ put cache'

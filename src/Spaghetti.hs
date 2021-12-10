{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides an entry point to the Weeder executable.
module Spaghetti (main, mainWithConfig) where

import Control.Monad.RWS
import Control.Monad.State
import Data.Foldable
import Data.List (isSuffixOf)
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
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, withCurrentDirectory)
import System.Exit (exitFailure)
import System.FilePath (isExtensionOf)
import UniqSupply

-- | Parse command line arguments and into a 'Config' and run 'mainWithConfig'.
main :: IO ()
main = do
  putStrLn "start"
  (hieExt, hieDirectories, requireHsFiles) <-
    execParser $
      info (optsP <**> helper <**> versionP) mempty

  mainWithConfig hieExt hieDirectories requireHsFiles
  where
    optsP =
      (,,)
        <$> strOption
          ( long "hie-extension"
              <> value ".hie"
              <> help "Extension of HIE files"
              <> showDefault
          )
        <*> many
          ( strOption
              ( long "hie-directory"
                  <> help "A directory to look for .hie files in. Maybe specified multiple times. Default ./."
              )
          )
        <*> switch
          ( long "require-hs-files"
              <> help "Requries that all .hie files have matching .hs files. This can help deal with skipping .hie files for Haskell modules that have since been removed"
          )

    versionP =
      infoOption
        ( "weeder version "
            <> showVersion version
            <> "\nhie version "
            <> show hieVersion
        )
        (long "version" <> help "Show version")

-- | Run Weeder in the current working directory with a given 'Config'.
--
-- This will recursively find all files with the given extension in the given directories, perform
-- analysis, and report all unused definitions according to the 'Config'.
mainWithConfig :: String -> [FilePath] -> Bool -> IO ()
mainWithConfig hieExt hieDirectories requireHsFiles = do
  putStrLn "Looking for hie files..."
  hieFilePaths <-
    concat
      <$> traverse
        (getFilesIn hieExt)
        ( if null hieDirectories
            then ["./."]
            else hieDirectories
        )

  hsFilePaths <-
    if requireHsFiles
      then getFilesIn ".hs" "./."
      else pure []

  nameCache <- do
    uniqSupply <- mkSplitUniqSupply 'z'
    return (initNameCache uniqSupply [])

  hieFiles <- flip evalStateT nameCache $ do
    forM hieFilePaths $ \hieFilePath -> do
      liftIO $ putStrLn $ "reading " <> hieFilePath
      readCompatibleHieFileOrExit hieFilePath
  -- let hsFileExists = any (hie_hs_file hieFileResult `isSuffixOf`) hsFilePaths
  -- when (not requireHsFiles || hsFileExists) $ do
  --   -- liftIO $ T.putStrLn $ runPrinter $ hieFile hieFileResult
  --   liftIO $ T.putStrLn $ runPrinter $ topLevel hieFileResult

  let (mods, env) = flip runState mempty $ mapM parseModule hieFiles
  liftIO $ T.writeFile "spaghetti.dot" $ runPrinter $ renderGraphViz env mods

-- mapM_ print hieFilePaths
-- mapM_ print hsFilePaths

-- | Recursively search for files with the given extension in given directory
getFilesIn ::
  -- | Only files with this extension are considered
  String ->
  -- | Directory to look in
  FilePath ->
  IO [FilePath]
getFilesIn ext path = do
  exists <-
    doesPathExist path

  if exists
    then do
      isFile <-
        doesFileExist path

      if isFile && ext `isExtensionOf` path
        then do
          path' <-
            canonicalizePath path

          return [path']
        else do
          isDir <-
            doesDirectoryExist path

          if isDir
            then do
              cnts <-
                listDirectory path

              withCurrentDirectory path (foldMap (getFilesIn ext) cnts)
            else return []
    else return []

-- | Read a .hie file, exiting if it's an incompatible version.
readCompatibleHieFileOrExit :: FilePath -> StateT NameCache IO HieFile
readCompatibleHieFileOrExit path = do
  nameCache <- get
  res <- liftIO $ readHieFileWithVersion (\(v, _) -> v == hieVersion) nameCache path
  case res of
    Right (HieFileResult {hie_file_result}, nameCache') -> hie_file_result <$ put nameCache'
    Left (v, _ghcVersion) -> liftIO $ do
      putStrLn $ "incompatible hie file: " <> path
      putStrLn $
        "    this version of weeder was compiled with GHC version "
          <> show hieVersion
      putStrLn $
        "    the hie files in this project were generated with GHC version "
          <> show v
      putStrLn $
        "    weeder must be built with the same GHC version"
          <> " as the project it is used on"
      exitFailure

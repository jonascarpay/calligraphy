{-# LANGUAGE NamedFieldPuns #-}

module Calligraphy.Phases.Search
  ( searchFiles,
    pSearchConfig,
    SearchConfig,
  )
where

import qualified Calligraphy.Compat.GHC as GHC
import Calligraphy.Compat.Lib (readHieFileCompat)
import Control.Applicative
import Control.Monad.State
import Data.IORef
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, toList)
import Options.Applicative hiding (str)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath (isExtensionOf, (</>))

searchFiles :: SearchConfig -> IO [GHC.HieFile]
searchFiles SearchConfig {searchDotPaths, searchRoots, includePatterns, excludePatterns} = do
  hieFilePaths <- searchHieFilePaths

  hieFiles <- do
    uniqSupply <- GHC.mkSplitUniqSupply 'z'
    ref <- newIORef (GHC.initNameCache uniqSupply [])
    forM hieFilePaths (readHieFileWithWarning ref)

  pure $
    flip filter hieFiles $ \file ->
      let matches pat =
            matchPattern pat (GHC.moduleNameString . GHC.moduleName . GHC.hie_module $ file)
              || matchPattern pat (GHC.hie_hs_file file)
       in maybe True (any matches) includePatterns && maybe True (not . any matches) excludePatterns
  where
    searchHieFilePaths = do
      roots <- mapM makeAbsolute (maybe ["./."] toList searchRoots)
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

readHieFileWithWarning :: IORef GHC.NameCache -> FilePath -> IO GHC.HieFile
readHieFileWithWarning ref path = do
  GHC.HieFileResult fileHieVersion fileGHCVersion hie <- readHieFileCompat ref path
  when (GHC.hieVersion /= fileHieVersion) $ do
    putStrLn $ "WARNING: version mismatch in " <> path
    putStrLn $ "    The hie files in this project were generated with GHC version: " <> show fileGHCVersion
    putStrLn $ "    This version of calligraphy was compiled with GHC version: " <> show GHC.hieVersion
    putStrLn $ "    Optimistically continuing anyway..."
  pure hie

data SearchConfig = SearchConfig
  { includePatterns :: Maybe (NonEmpty Pattern),
    excludePatterns :: Maybe (NonEmpty Pattern),
    searchDotPaths :: Bool,
    searchRoots :: Maybe (NonEmpty FilePath)
  }

newtype Pattern = Pattern String

matchPattern :: Pattern -> String -> Bool
matchPattern (Pattern matcher) = go False matcher
  where
    go _ ('*' : ms) cs = go True ms cs
    go _ (m : ms) (c : cs) | m == c = go False ms cs || go True (m : ms) cs
    go True ms (_ : cs) = go True ms cs
    go _ [] [] = True
    go _ _ _ = False

pSearchConfig :: Parser SearchConfig
pSearchConfig =
  SearchConfig
    <$> (fmap nonEmpty . many)
      ( Pattern
          <$> strArgument
            ( metavar "MODULE"
                <> help "Name or filepath of a module to include in the call graph. Can contain '*' wildcards. Defaults to '*'."
            )
      )
    <*> (fmap nonEmpty . many)
      ( Pattern
          <$> strOption
            ( long "exclude"
                <> short 'e'
                <> metavar "MODULE"
                <> help "Name or filepath of a module to exclude in the call graph. Can contain '*' wildcards."
            )
      )
    <*> switch (long "hidden" <> help "Search paths with a leading period. Disabled by default.")
    <*> (fmap nonEmpty . many)
      ( strOption
          ( long "input"
              <> short 'i'
              <> metavar "PATH"
              <> help "Filepaths to search for HIE files. If passed a file, it will be processed as is. If passed a directory, the directory will be searched recursively. Can be repeated. Defaults to './.'"
          )
      )

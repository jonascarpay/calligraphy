{-# LANGUAGE NamedFieldPuns #-}

module Search (searchFiles, pSearchConfig, SearchConfig) where

import qualified Compat as GHC
import Control.Applicative
import Control.Monad.State
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Options.Applicative hiding (str)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath (isExtensionOf, (</>))

searchFiles :: SearchConfig -> IO [GHC.HieFile]
searchFiles SearchConfig {searchDotPaths, searchRoots, includeFilters, excludeFilters} = do
  hieFilePaths <- searchHieFilePaths
  nameCache <- do
    uniqSupply <- GHC.mkSplitUniqSupply 'z'
    return (GHC.initNameCache uniqSupply [])

  hieFiles <-
    flip evalStateT nameCache $
      forM hieFilePaths readHieFileWithWarning

  pure $
    flip filter hieFiles $ \file ->
      let modName = GHC.moduleNameString . GHC.moduleName . GHC.hie_module $ file
       in maybe True (not . any (flip matchFilter modName)) excludeFilters && maybe True (any (flip matchFilter modName)) includeFilters
  where
    searchHieFilePaths = do
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

readHieFileWithWarning :: FilePath -> StateT GHC.NameCache IO GHC.HieFile
readHieFileWithWarning path = do
  nameCache <- get
  (GHC.HieFileResult fileHieVersion fileGHCVersion hie, cache') <- liftIO $ GHC.readHieFile nameCache path
  when (GHC.hieVersion /= fileHieVersion) $
    liftIO $ do
      putStrLn $ "WARNING: version mismatch in " <> path
      putStrLn $ "    The hie files in this project were generated with GHC version: " <> show fileGHCVersion
      putStrLn $ "    This version of calligraphy was compiled with GHC version: " <> show GHC.hieVersion
      putStrLn $ "    Optimistically continuing anyway..."
  hie <$ put cache'

data SearchConfig = SearchConfig
  { searchDotPaths :: Bool,
    searchRoots :: [FilePath],
    includeFilters :: Maybe (NonEmpty Filter), -- TODO this should be a Maybe NonEmpty
    excludeFilters :: Maybe (NonEmpty Filter) -- TODO this should be a Maybe NonEmpty
  }

newtype Filter = Filter String

matchFilter :: Filter -> String -> Bool
matchFilter (Filter matcher) = go False matcher
  where
    go _ ('*' : ms) cs = go True ms cs
    go _ (m : ms) (c : cs) | m == c = go False ms cs || go True (m : ms) cs
    go True ms (_ : cs) = go True ms cs
    go _ [] [] = True
    go _ _ _ = False

pSearchConfig :: Parser SearchConfig
pSearchConfig =
  SearchConfig
    <$> switch
      ( long "hidden"
          <> help "Search paths with a leading period. Disabled by default."
      )
    <*> many
      ( strOption
          ( long "input"
              <> short 'i'
              <> metavar "PATH"
              <> help "Filepaths to search. If passed a file, it will be processed as is. If passed a directory, the directory will be searched recursively. Can be repeated. Defaults to ./."
          )
      )
    <*> (fmap nonEmpty . many)
      ( Filter
          <$> strOption
            ( long "module"
                <> short 'm'
                <> metavar "PATTERN"
                <> help "Only include modules that match the specified pattern. Can contain '*' wildcards. Can be repeated."
            )
      )
    <*> (fmap nonEmpty . many)
      ( Filter
          <$> strOption
            ( long "exclude"
                <> short 'e'
                <> metavar "PATTERN"
                <> help "Exclude modules that match the specified pattern. Can contain '*' wildcards. Can be repeated."
            )
      )

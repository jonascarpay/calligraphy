{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calligraphy (main, mainWithConfig) where

import Collapse
import qualified Compat as GHC
import Control.Monad.RWS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Text
import Data.Version (showVersion)
import Debug
import EdgeFilter
import Filter
import GraphViz
import Options.Applicative
import Parse
import Paths_calligraphy (version)
import Printer
import Search
import System.Directory (findExecutable)
import System.Exit
import System.IO (stderr)
import System.Process

main :: IO ()
main = do
  config <- execParser $ info (pConfig <**> helper <**> versionP) mempty
  mainWithConfig config
  where
    versionP =
      infoOption
        ( "calligraphy version "
            <> showVersion version
            <> "\nhie version "
            <> show GHC.hieVersion
        )
        (long "version" <> help "Show version")

printStderr :: Printer () -> IO ()
printStderr = Text.hPutStrLn stderr . runPrinter

printDie :: Printer () -> IO a
printDie txt = printStderr txt >> exitFailure

mainWithConfig :: AppConfig -> IO ()
mainWithConfig AppConfig {searchConfig, renderConfig, outputConfig, edgeFilterConfig, debugConfig, collapseConfig, filterConfig} = do
  let debug :: (DebugConfig -> Bool) -> Printer () -> IO ()
      debug fp printer = when (fp debugConfig) (printStderr printer)

  hieFiles <- searchFiles searchConfig
  when (null hieFiles) $ die "No files matched your search criteria.."
  debug dumpHieFile $ mapM_ ppHieFile hieFiles

  (modulesDebug, modules) <- either (printDie . ppParseError) pure (parseHieFiles hieFiles)
  debug dumpLexicalTree $ ppModulesDebugInfo modulesDebug
  debug dumpParsedModules $ ppModules modules

  let modulesCollapsed = collapse collapseConfig modules
  debug dumpCollapsedModules $ ppModules modulesCollapsed

  let modulesEdgeFiltered = filterEdges edgeFilterConfig modulesCollapsed
  modulesFiltered <- either (printDie . ppFilterError) pure $ filterModules filterConfig modulesEdgeFiltered
  debug dumpFilteredModules $ ppModules modulesFiltered

  let txt = runPrinter $ render renderConfig modulesFiltered

  output outputConfig txt

data AppConfig = AppConfig
  { searchConfig :: SearchConfig,
    collapseConfig :: CollapseConfig,
    edgeFilterConfig :: EdgeFilterConfig,
    filterConfig :: FilterConfig,
    renderConfig :: RenderConfig,
    outputConfig :: OutputConfig,
    debugConfig :: DebugConfig
  }

pConfig :: Parser AppConfig
pConfig =
  AppConfig <$> pSearchConfig
    <*> pCollapseConfig
    <*> pEdgeFilterConfig
    <*> pFilterConfig
    <*> pRenderConfig
    <*> pOutputConfig
    <*> pDebugConfig

output :: OutputConfig -> Text -> IO ()
output cfg@OutputConfig {outputDotPath, outputPngPath, outputStdout} txt = do
  unless (hasOutput cfg) $ Text.hPutStrLn stderr "Warning: no output options specified, run with --help to see options"
  forM_ outputDotPath $ \fp -> Text.writeFile fp txt
  forM_ outputPngPath writePng
  when outputStdout $ Text.putStrLn txt
  where
    hasOutput (OutputConfig Nothing Nothing False) = False
    hasOutput _ = True

    writePng fp = do
      mexe <- findExecutable "dot"
      case mexe of
        Nothing -> die "no dot"
        Just exe -> do
          (code, out, err) <- readProcessWithExitCode exe ["-Tpng", "-o", fp] (T.unpack txt)
          unless (code == ExitSuccess) $ do
            putStrLn "dot crashed"
            putStrLn out
            putStrLn err

data OutputConfig = OutputConfig
  { outputDotPath :: Maybe FilePath,
    outputPngPath :: Maybe FilePath,
    outputStdout :: Bool
  }

-- TODO allow output to multiple places
pOutputConfig :: Parser OutputConfig
pOutputConfig =
  OutputConfig
    <$> optional (strOption (short 'd' <> long "output-dot" <> metavar "FILE" <> help ".dot output path"))
    <*> optional (strOption (short 'p' <> long "output-png" <> metavar "FILE" <> help ".png output path (requires 'dot' executable in PATH)"))
    <*> switch (long "output-stdout" <> help "Output to stdout")

data DebugConfig = DebugConfig
  { dumpHieFile :: Bool,
    dumpLexicalTree :: Bool,
    dumpParsedModules :: Bool,
    dumpCollapsedModules :: Bool,
    dumpFilteredModules :: Bool
  }

pDebugConfig :: Parser DebugConfig
pDebugConfig =
  DebugConfig
    <$> switch (long "ddump-hie-file" <> help "Debug dump raw HIE files.")
    <*> switch (long "ddump-lexical-tree" <> help "Debug dump the reconstructed lexical structure of HIE files, the intermediate output in the parsing phase.")
    <*> switch (long "ddump-parsed" <> help "Debug dump the final parsing phase output, i.e. reconstructed and cleaned up AST.")
    <*> switch (long "ddump-collapsed" <> help "Debug dump ASTs after the collapsing phase.")
    <*> switch (long "ddump-filtered" <> help "Debug dump ASTs after the filtering phase.")

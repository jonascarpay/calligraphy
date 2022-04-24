{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Calligraphy (main, mainWithConfig) where

import Calligraphy.Compat.Debug (ppHieFile)
import qualified Calligraphy.Compat.GHC as GHC
import Calligraphy.Phases.Collapse
import Calligraphy.Phases.DependencyFilter
import Calligraphy.Phases.EdgeCleanup
import Calligraphy.Phases.NodeFilter
import Calligraphy.Phases.Parse
import Calligraphy.Phases.Render
import Calligraphy.Phases.Search
import Calligraphy.Util.Printer
import Calligraphy.Util.Types (ppCallGraph)
import Control.Monad.RWS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Text
import Data.Version (showVersion)
import Options.Applicative
import Paths_calligraphy (version)
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

mainWithConfig :: AppConfig -> IO ()
mainWithConfig AppConfig {..} = do
  let debug :: (DebugConfig -> Bool) -> Printer () -> IO ()
      debug fp printer = when (fp debugConfig) (printStderr printer)

  hieFiles <- searchFiles searchConfig
  when (null hieFiles) $ die "No files matched your search criteria.."
  debug dumpHieFile $ mapM_ ppHieFile hieFiles

  (parsePhaseDebug, cgParsed) <- either (printDie . ppParseError) pure (parseHieFiles parseConfig hieFiles)
  debug dumpLexicalTree $ ppParsePhaseDebugInfo parsePhaseDebug
  let cgCollapsed = collapse collapseConfig cgParsed
  let cgNodeFiltered = filterNodes nodeFilterConfig cgCollapsed
  cgDependencyFiltered <- either (printDie . ppFilterError) pure $ dependencyFilter dependencyFilterConfig cgNodeFiltered
  let cgCleaned = cleanupEdges edgeFilterConfig cgDependencyFiltered
  debug dumpFinal $ ppCallGraph cgCleaned

  let txt = runPrinter $ render renderConfig cgCleaned

  output outputConfig txt

data AppConfig = AppConfig
  { searchConfig :: SearchConfig,
    parseConfig :: ParseConfig,
    collapseConfig :: CollapseConfig,
    dependencyFilterConfig :: DependencyFilterConfig,
    edgeFilterConfig :: EdgeCleanupConfig,
    nodeFilterConfig :: NodeFilterConfig,
    renderConfig :: RenderConfig,
    outputConfig :: OutputConfig,
    debugConfig :: DebugConfig
  }

printStderr :: Printer () -> IO ()
printStderr = Text.hPutStrLn stderr . runPrinter

printDie :: Printer () -> IO a
printDie txt = printStderr txt >> exitFailure

pConfig :: Parser AppConfig
pConfig =
  AppConfig <$> pSearchConfig
    <*> pParseConfig
    <*> pCollapseConfig
    <*> pDependencyFilterConfig
    <*> pEdgeCleanupConfig
    <*> pNodeFilterConfig
    <*> pRenderConfig
    <*> pOutputConfig
    <*> pDebugConfig

output :: OutputConfig -> Text -> IO ()
output cfg@OutputConfig {..} txt = do
  unless (hasOutput cfg) $ Text.hPutStrLn stderr "Warning: no output options specified, run with --help to see options"
  forM_ outputDotPath $ \fp -> Text.writeFile fp txt
  forM_ outputPngPath writePng
  when outputStdout $ Text.putStrLn txt
  where
    hasOutput (OutputConfig Nothing Nothing False) = False
    hasOutput _ = True

    writePng (fp, cmd) = do
      mexe <- findExecutable cmd
      case mexe of
        Nothing -> die $ "Unable to find '" <> cmd <> "' executable! Make sure it is installed, or use another output method/engine."
        Just exe -> do
          (code, out, err) <- readProcessWithExitCode exe ["-Tpng", "-o", fp] (T.unpack txt)
          unless (code == ExitSuccess) $ do
            putStrLn $ cmd <> " crashed:"
            putStrLn out
            putStrLn err

data OutputConfig = OutputConfig
  { outputDotPath :: Maybe FilePath,
    outputPngPath :: Maybe (FilePath, String),
    outputStdout :: Bool
  }

pOutputConfig :: Parser OutputConfig
pOutputConfig =
  OutputConfig
    <$> optional (strOption (short 'd' <> long "output-dot" <> metavar "FILE" <> help ".dot output path"))
    <*> optional
      ( liftA2
          (,)
          (strOption (short 'p' <> long "output-png" <> metavar "FILE" <> help ".png output path (requires 'dot' executable in PATH)"))
          (strOption (long "render-engine" <> metavar "CMD" <> help "Render engine to use with --output-png" <> value "dot" <> showDefault))
      )
    <*> switch (long "output-stdout" <> help "Output to stdout")

data DebugConfig = DebugConfig
  { dumpHieFile :: Bool,
    dumpLexicalTree :: Bool,
    dumpFinal :: Bool
  }

pDebugConfig :: Parser DebugConfig
pDebugConfig =
  DebugConfig
    <$> switch (long "ddump-hie-file" <> help "Debug dump raw HIE files.")
    <*> switch (long "ddump-lexical-tree" <> help "Debug dump the reconstructed lexical structure of HIE files, the intermediate output in the parsing phase.")
    <*> switch (long "ddump-final" <> help "Debug dump the final tree after processing, i.e. as it will be rendered.")

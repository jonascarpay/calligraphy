{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Calligraphy (main, mainWithConfig) where

import Calligraphy.Compat.Debug (ppHieFile)
import qualified Calligraphy.Compat.GHC as GHC
import Calligraphy.Phases.DependencyFilter
import Calligraphy.Phases.EdgeCleanup
import Calligraphy.Phases.NodeFilter
import Calligraphy.Phases.Parse
import Calligraphy.Phases.Render.Common
import Calligraphy.Phases.Render.GraphViz
import Calligraphy.Phases.Render.Mermaid
import Calligraphy.Phases.Search
import Calligraphy.Util.Printer
import Calligraphy.Util.Types (ppCallGraph)
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

  (parsePhaseDebug, cgParsed) <- either (printDie . ppParseError) pure (parseHieFiles hieFiles)
  debug dumpLexicalTree $ ppParsePhaseDebugInfo parsePhaseDebug
  let cgCollapsed = filterNodes nodeFilterConfig cgParsed
  cgDependencyFiltered <- either (printDie . ppFilterError) pure $ dependencyFilter dependencyFilterConfig cgCollapsed
  let cgCleaned = cleanupEdges edgeFilterConfig cgDependencyFiltered
  debug dumpFinal $ ppCallGraph cgCleaned

  let renderConfig' = renderConfig {clusterModules = clusterModules renderConfig && not (collapseModules nodeFilterConfig)}
  renderable <- either (printDie . ppRenderError) pure (renderGraph renderConfig' cgCleaned)

  output
    outputConfig
    (runPrinter $ renderGraphViz graphVizConfig renderable)
    (runPrinter $ renderMermaid renderable)

data AppConfig = AppConfig
  { searchConfig :: SearchConfig,
    nodeFilterConfig :: NodeFilterConfig,
    dependencyFilterConfig :: DependencyFilterConfig,
    edgeFilterConfig :: EdgeCleanupConfig,
    renderConfig :: RenderConfig,
    graphVizConfig :: GraphVizConfig,
    outputConfig :: OutputConfig,
    debugConfig :: DebugConfig
  }

printStderr :: Printer () -> IO ()
printStderr = Text.hPutStrLn stderr . runPrinter

printDie :: Printer () -> IO a
printDie txt = printStderr txt >> exitFailure

pConfig :: Parser AppConfig
pConfig =
  AppConfig
    <$> pSearchConfig
    <*> pNodeFilterConfig
    <*> pDependencyFilterConfig
    <*> pEdgeCleanupConfig
    <*> pRenderConfig
    <*> pGraphVizConfig
    <*> pOutputConfig
    <*> pDebugConfig

output :: OutputConfig -> Text -> Text -> IO ()
output cfg@OutputConfig {..} dotTxt mermaidTxt = do
  unless (hasOutput cfg) $ Text.hPutStrLn stderr "Warning: no output options specified, run with --help to see options"
  forM_ outputDotPath $ \fp -> Text.writeFile fp dotTxt
  forM_ outputPngPath $ \fp -> runDot ["-Tpng", "-o", fp]
  forM_ outputSvgPath $ \fp -> runDot ["-Tsvg", "-o", fp]
  forM_ outputMermaidPath $ \fp -> Text.writeFile fp mermaidTxt
  case outputStdout of
    StdoutDot -> Text.putStrLn dotTxt
    StdoutMermaid -> Text.putStrLn mermaidTxt
    StdoutNone -> pure ()
  where
    hasOutput (OutputConfig Nothing Nothing Nothing Nothing _ StdoutNone) = False
    hasOutput _ = True

    runDot flags = do
      mexe <- findExecutable outputEngine
      case mexe of
        Nothing -> die $ "Unable to find '" <> outputEngine <> "' executable! Make sure it is installed, or use another output method/engine."
        Just exe -> do
          (code, out, err) <- readProcessWithExitCode exe flags (T.unpack dotTxt)
          unless (code == ExitSuccess) $ do
            putStrLn $ outputEngine <> " crashed:"
            putStrLn out
            putStrLn err

data StdoutFormat = StdoutNone | StdoutDot | StdoutMermaid

data OutputConfig = OutputConfig
  { outputDotPath :: Maybe FilePath,
    outputPngPath :: Maybe FilePath,
    outputSvgPath :: Maybe FilePath,
    outputMermaidPath :: Maybe FilePath,
    outputEngine :: String,
    outputStdout :: StdoutFormat
  }

pOutputConfig :: Parser OutputConfig
pOutputConfig =
  OutputConfig
    <$> optional (strOption (long "output-dot" <> short 'd' <> metavar "FILE" <> help ".dot output path"))
    <*> optional (strOption (long "output-png" <> short 'p' <> metavar "FILE" <> help ".png output path (requires `dot` or other engine in PATH)"))
    <*> optional (strOption (long "output-svg" <> short 's' <> metavar "FILE" <> help ".svg output path (requires `dot` or other engine in PATH)"))
    <*> optional (strOption (long "output-mermaid" <> short 'm' <> metavar "FILE" <> help "Mermaid output path"))
    <*> strOption (long "render-engine" <> metavar "CMD" <> help "Render engine to use with --output-png and --output-svg" <> value "dot" <> showDefault)
    <*> pStdoutFormat

pStdoutFormat :: Parser StdoutFormat
pStdoutFormat =
  flag' StdoutDot (long "stdout-dot" <> help "Output graphviz dot to stdout")
    <|> flag' StdoutMermaid (long "stdout-mermaid" <> help "Output Mermaid to stdout")
    <|> pure StdoutNone

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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Calligraphy (main, mainWithConfig) where

import Calligraphy.Compat.Debug (ppHieFile)
import qualified Calligraphy.Compat.GHC as GHC
import Calligraphy.Phases.DependencyFilter
import Calligraphy.Phases.EdgeCleanup
import Calligraphy.Phases.NodeFilter
import Calligraphy.Phases.Parse
import Calligraphy.Phases.Render
import Calligraphy.Phases.Search
import Calligraphy.Util.Printer
import Calligraphy.Util.Types (CallGraph (CallGraph), Key (Key), encodeModuleTree, moduleTree, ppCallGraph)
import Control.Monad.RWS
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import Data.FileEmbed (embedFile)
import Data.Foldable (toList)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.IO as Text
import Data.Tuple (swap)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Options.Applicative
import Paths_calligraphy (version)
import System.Directory (findExecutable)
import System.Exit
import System.IO (IOMode (WriteMode), stderr, withFile)
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

  output outputConfig renderConfig' cgCleaned

data AppConfig = AppConfig
  { searchConfig :: SearchConfig,
    nodeFilterConfig :: NodeFilterConfig,
    dependencyFilterConfig :: DependencyFilterConfig,
    edgeFilterConfig :: EdgeCleanupConfig,
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
    <*> pNodeFilterConfig
    <*> pDependencyFilterConfig
    <*> pEdgeCleanupConfig
    <*> pRenderConfig
    <*> pOutputConfig
    <*> pDebugConfig

data JSONInfo = JSONInfo
  { _jsonTree :: ParsePhaseDebugInfo,
    _jsonGraph :: CallGraph
  }
  deriving (Generic)

d3Script :: ByteString
d3Script = $(embedFile "js/d3.v7.min.js")

treegraphScript :: ByteString
treegraphScript = $(embedFile "js/treegraph.js")

mainScript :: ByteString
mainScript = $(embedFile "js/main.js")

htmlScript :: Lazy.ByteString -> Builder
htmlScript json =
  unlinesBuilder $
    [ Builder.byteString "<!doctype html>",
      Builder.byteString "<html>",
      Builder.byteString "<head>",
      Builder.byteString "<meta charset=\"utf8\">",
      Builder.byteString "<script>" <> Builder.byteString d3Script <> Builder.byteString "</script>",
      Builder.byteString "<script>" <> Builder.byteString treegraphScript <> Builder.byteString "</script>",
      Builder.byteString "<script type=\"application/json\" id=\"treegraph-json\">" <> Builder.lazyByteString json <> Builder.byteString "</script>",
      Builder.byteString "</head>",
      Builder.byteString "<body>",
      Builder.byteString "<div id=\"view\"><div>",
      Builder.byteString "<a id=\"download\" href=\"#\">Download SVG</a>",
      Builder.byteString "<script>" <> Builder.byteString mainScript <> "</script>",
      Builder.byteString "</body>",
      Builder.byteString "</html>"
    ]
  where
    unlinesBuilder xs = mconcat (fmap (\a -> a <> Builder.charUtf8 '\n') xs)

encodeCallGraph :: CallGraph -> Aeson.Value
encodeCallGraph graph@(CallGraph _ calls' types') =
  let encodeEdge (Key source, Key target) =
        Aeson.object
          [ (fromString "source", Aeson.toJSON source),
            (fromString "target", Aeson.toJSON target)
          ]
      tree = encodeModuleTree $ moduleTree graph
      calls = Aeson.toJSON $ fmap (encodeEdge . swap) $ toList calls'
      types = Aeson.toJSON $ fmap (encodeEdge . swap) $ toList types'
   in Aeson.object
        [ (fromString "tree", tree),
          (fromString "calls", calls),
          (fromString "types", types)
        ]

output :: OutputConfig -> RenderConfig -> CallGraph -> IO ()
output cfg@OutputConfig {..} renderConfig' graph = do
  unless (hasOutput cfg) $ Text.hPutStrLn stderr "Warning: no output options specified, run with --help to see options"
  forM_ outputDotPath $ \fp -> Text.writeFile fp txt
  forM_ outputPngPath $ \fp -> runDot ["-Tpng", "-o", fp]
  forM_ outputSvgPath $ \fp -> runDot ["-Tsvg", "-o", fp]
  forM_ outputJsonPath $ \fp -> Aeson.encodeFile fp json
  forM_ outputHtmlPath $ \fp -> withFile fp WriteMode $ \f ->
    Builder.hPutBuilder f (htmlScript $ Aeson.encode json)
  when outputStdout $ Text.putStrLn txt
  where
    txt = runPrinter $ render renderConfig' graph
    json = encodeCallGraph graph
    hasOutput (OutputConfig Nothing Nothing Nothing Nothing Nothing _ False) = False
    hasOutput _ = True

    runDot flags = do
      mexe <- findExecutable outputEngine
      case mexe of
        Nothing -> die $ "Unable to find '" <> outputEngine <> "' executable! Make sure it is installed, or use another output method/engine."
        Just exe -> do
          (code, out, err) <- readProcessWithExitCode exe flags (T.unpack txt)
          unless (code == ExitSuccess) $ do
            putStrLn $ outputEngine <> " crashed:"
            putStrLn out
            putStrLn err

data OutputConfig = OutputConfig
  { outputDotPath :: Maybe FilePath,
    outputPngPath :: Maybe FilePath,
    outputSvgPath :: Maybe FilePath,
    outputHtmlPath :: Maybe FilePath,
    outputJsonPath :: Maybe FilePath,
    outputEngine :: String,
    outputStdout :: Bool
  }

pOutputConfig :: Parser OutputConfig
pOutputConfig =
  OutputConfig
    <$> optional (strOption (long "output-dot" <> short 'd' <> metavar "FILE" <> help ".dot output path"))
    <*> optional (strOption (long "output-png" <> short 'p' <> metavar "FILE" <> help ".png output path (requires `dot` or other engine in PATH)"))
    <*> optional (strOption (long "output-svg" <> short 's' <> metavar "FILE" <> help ".svg output path (requires `dot` or other engine in PATH)"))
    <*> optional (strOption (long "output-html" <> long "html" <> metavar "FILE" <> help ".html output path"))
    <*> optional (strOption (long "output-json" <> short 'j' <> metavar "FILE" <> help ".json output path"))
    <*> strOption (long "render-engine" <> metavar "CMD" <> help "Render engine to use with --output-png and --output-svg" <> value "dot" <> showDefault)
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

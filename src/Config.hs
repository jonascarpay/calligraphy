module Config
  ( RenderConfig (..),
    SearchConfig (..),
    OutputConfig (..),
    AppConfig (..),
    pConfig,
    RenderLevel (..),
    Filter,
    match,
  )
where

import Options.Applicative

newtype Filter = Filter String

match :: Filter -> String -> Bool
match (Filter matcher) = go False matcher
  where
    go _ ('*' : ms) cs = go True ms cs
    go _ (m : ms) (c : cs) | m == c = go False ms cs || go True (m : ms) cs
    go True ms (_ : cs) = go True ms cs
    go _ [] [] = True
    go _ _ _ = False

data AppConfig = AppConfig
  { searchConfig :: SearchConfig,
    renderConfig :: RenderConfig,
    outputConfig :: OutputConfig
  }

data SearchConfig = SearchConfig
  { searchDotPaths :: Bool,
    searchRoots :: [FilePath]
  }

-- TODO no clusters
-- TODO sourceLocs
-- TODO qualified names
-- TODO LR rankdir
-- TODO move to Render module
-- TODO experiment with arrow direction (rendering, ordering)
-- TODO these are mostly "filtering" options, with the exception of splines
--   alternative, splines is a graphviz option
data RenderConfig = RenderConfig
  { renderLevel :: RenderLevel,
    showCalls :: Bool,
    splines :: Bool,
    includeFilters :: [Filter], -- TODO this should be a Maybe NonEmpty
    excludeFilters :: [Filter] -- TODO this should be a Maybe NonEmpty
  }

data OutputConfig
  = OutputStdOut
  | OutputFile FilePath
  | OutputPng FilePath

data RenderLevel
  = All
  | Module
  | Exports
  deriving (Eq, Show)

pConfig :: Parser AppConfig
pConfig = AppConfig <$> pSearchConfig <*> pRenderConfig <*> pOutputConfig

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
              <> help "Filepaths to search. If passed a file, it will be processed as is. If passed a directory, the directory will be searched recursively. Can be specified multiple times. Defaults to ./."
          )
      )

pRenderLevel :: Parser RenderLevel
pRenderLevel =
  flag' Module (long "only-toplevel" <> help "Only render top-level bindings")
    <|> flag' Exports (long "only-exports" <> help "Only render exported bindings")
    <|> pure All

pRenderConfig :: Parser RenderConfig
pRenderConfig =
  RenderConfig
    <$> pRenderLevel
    <*> flag True False (long "hide-calls" <> help "Don't show function call arrows")
    <*> switch (long "splines" <> help "Render arrows as splines")
    <*> many
      ( Filter
          <$> strOption
            ( long "module"
                <> short 'm'
                <> help "Only include modules that match the specified pattern. Can contain '*' wildcards. Can be specified multiple times"
            )
      )
    <*> many
      ( Filter
          <$> strOption
            ( long "exclude"
                <> short 'e'
                <> help "Exclude modules that match the specified pattern. Can contain '*' wildcards. Can be specified multiple times"
            )
      )

-- TODO allow output to multiple places
pOutputConfig :: Parser OutputConfig
pOutputConfig =
  OutputFile <$> strOption (long "output-dot")
    <|> OutputPng <$> strOption (long "output-png")
    <|> pure OutputStdOut

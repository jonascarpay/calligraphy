module Config where

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

data SearchConfig = SearchConfig
  { searchDotDirs :: Bool,
    searchRoots :: [FilePath],
    requireHsFiles :: Bool
  }

pSearchConfig :: Parser SearchConfig
pSearchConfig =
  SearchConfig
    <$> flag
      True
      False
      ( long "search-dot-dirs"
          <> help "Search through directories with a leading period"
      )
    <*> many
      ( strOption
          ( long "dir"
              <> help "Directory to search in. Can be specified mutliple times. Defaults to ./."
          )
      )
    <*> flag True False (long "require-hs-files" <> help "Requires that all .hie files have matching .hs files. Useful for skippng .hie files for modules that have since been removed.")

data RenderConfig = RenderConfig
  { hideLocalBindings :: RenderLevel,
    showCalls :: Bool,
    splines :: Bool,
    moduleIncludes :: [Filter],
    moduleExcludes :: [Filter]
  }

data RenderLevel
  = All
  | Module
  | Exports

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
                <> short 'i'
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

module Filter where

import Data.EnumMap (EnumMap)
import Data.EnumSet (EnumSet)
import Data.List.NonEmpty (NonEmpty)
import Parse qualified
import Prelude hiding (filter)

newtype Key = Key {unKey :: Int}

filter :: FilterConfig -> [Parse.Modules] -> Graph
filter = undefined

data FilterConfig = FilterConfig
  { collapseValues :: Bool,
    collapseClasses :: Bool,
    collapseConstructors :: Bool,
    collapseData :: Bool,
    onlyExports :: Bool,
    depRoot :: Maybe [String],
    revDepRoot :: Maybe [String],
    moduleFilterIn :: Maybe (NonEmpty String),
    moduleFilterOut :: Maybe (NonEmpty String)
  }

data Graph = Graph
  { modulesModules :: [Module],
    modulesCalls :: EnumMap Key (EnumSet Key)
  }

data Module = Module
  { moduleName :: String,
    moduleDecls :: [Decl]
  }

data Decl = Decl
  { declName :: String,
    declKey :: Key,
    declType :: Parse.DeclType,
    declExport :: Bool,
    declChildren :: [Decl]
  }

-- Step 1: filter modules

filterModules :: Maybe (NonEmpty String) -> Maybe (NonEmpty String) -> [Parse.Modules] -> [Parse.Modules]
filterModules = undefined

{-# LANGUAGE NamedFieldPuns #-}

module Collapse (collapse, CollapseConfig, pCollapseConfig) where

import Control.Monad.State
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.Tree
import Options.Applicative
import Parse

data CollapseConfig = CollapseConfig
  { collapseValues :: Bool,
    collapseClasses :: Bool,
    collapseConstructors :: Bool,
    collapseData :: Bool
  }

collapse :: CollapseConfig -> Modules -> Modules
collapse CollapseConfig {collapseValues, collapseClasses, collapseConstructors, collapseData} Modules {modules, calls, inferences} =
  let (modules', reps) = flip runState mempty $ (traverse . traverse . traverse) go modules
   in Modules modules' (rekeyCalls reps calls) (rekeyCalls reps inferences)
  where
    shouldCollapse :: DeclType -> Bool
    shouldCollapse ValueDecl | collapseValues = True
    shouldCollapse ClassDecl | collapseClasses = True
    shouldCollapse ConDecl | collapseConstructors = True
    shouldCollapse DataDecl | collapseData = True
    shouldCollapse _ = False

    assoc :: Key -> Key -> State (EnumMap Key Key) ()
    assoc key rep = modify (EnumMap.insert key rep)

    go :: Tree Decl -> State (EnumMap Key Key) (Tree Decl)
    go (Node decl@(Decl _ key _ _ _) children) = do
      assoc key key
      if shouldCollapse (declType decl)
        then Node decl [] <$ (mapM_ . mapM_) (flip assoc key . declKey) children
        else Node decl <$> mapM go children

pCollapseConfig :: Parser CollapseConfig
pCollapseConfig =
  CollapseConfig
    <$> switch (long "collapse-values" <> short 'u' <> help "Collapse values, hiding local bindings")
    <*> switch (long "collapse-classes" <> help "Collapse classes, hiding class methods")
    <*> switch (long "collapse-constructors" <> help "Collapse consructors, hiding record accessors")
    <*> switch (long "collapse-data" <> help "Collapse data types, hiding constructors")

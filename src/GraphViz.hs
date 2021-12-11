{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GraphViz (render) where

import Config
import Control.Monad
import Data.Foldable
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Lib
import Printer (Printer, indent, strLn, textLn)

a :: Int
a = b
  where
    b = c
    c = a

collapseChildCalls :: Declaration -> Declaration
collapseChildCalls self@(Declaration name key loc scope _ _) = Declaration name key loc scope [] (collectCalls self)
  where
    collectCalls :: Declaration -> IntSet
    collectCalls decl = declCalls decl <> foldMap collectCalls (declSubs decl)

render :: RenderConfig -> [ParsedModule] -> Printer ()
render RenderConfig {renderLevel, showCalls, splines, includeFilters, excludeFilters} modulesUnfiltered = do
  textLn "digraph {"
  indent $ do
    unless splines $ strLn "splines=false;"
    textLn "// Module Clusters"
    forM_ (zip modules [0 :: Int ..]) $ \(mod, i) -> do
      strLn $ "subgraph cluster_" <> show i <> " {"
      indent $ do
        strLn $ "label=" <> show (pmName mod) <> ";"
        printModuleDeclarations mod
      textLn "}"
    when showCalls $ do
      textLn "// Call graph"
      forM_ calls $ \(caller, callee) -> strLn $ show caller <> " -> " <> show callee <> ";"
  textLn "}"
  where
    modules :: [ParsedModule]
    modules = include . exclude $ modulesUnfiltered
      where
        include = if null includeFilters then id else filter (matchesAny includeFilters)
        exclude = if null excludeFilters then id else filter (not . matchesAny excludeFilters)
        matchesAny l (ParsedModule name _ _) = any (flip match name) l

    calls :: [(Int, Int)]
    calls = nodes >>= collect
      where
        nodes = modules >>= declarationTree renderLevel
        collect (Declaration _ caller _ _ _ callees) = (caller,) <$> filter (flip IS.member callables) (IS.toList callees)
        callables = IS.fromList $ nodes >>= go
          where
            go (Declaration _ caller _ _ subs _) = caller : (subs >>= go)

    printModuleDeclarations :: ParsedModule -> Printer ()
    printModuleDeclarations mod@(ParsedModule _ exports binds) = mapM_ printDeclaration (declarationTree renderLevel mod)
      where
        bindSet = IS.fromList (declKey <$> binds)

        printDeclaration :: Declaration -> Printer ()
        printDeclaration (Declaration name key _ _ subs _) = do
          strLn $ show key <> " " <> declStyle key name <> ";"
          indent $
            forM_ subs $ \sub -> do
              strLn $ show key <> " -> " <> show (declKey sub) <> " [style=dashed, arrowhead=none];"
              printDeclaration sub

        declStyle :: Int -> String -> String
        declStyle key name
          | IS.member key exports = "[label=" <> name <> ", shape=box]"
          | IS.member key bindSet = "[label=" <> name <> ", shape=ellipse]"
          | otherwise = "[label=" <> name <> ", shape=ellipse, style=dashed]"

declarationTree :: RenderLevel -> ParsedModule -> [Declaration]
declarationTree rl (ParsedModule _ exports binds) =
  case rl of
    Exports -> collapseChildCalls <$> filter (flip IS.member exports . declKey) binds
    Module -> collapseChildCalls <$> binds
    All -> binds

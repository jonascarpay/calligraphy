{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module GraphViz (render) where

import Config
import Control.Monad
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Lib
import Printer

render :: IntMap Declaration -> RenderConfig -> [ParsedModule] -> Printer ()
render env RenderConfig {renderLevel, showCalls, splines, includeFilters, excludeFilters} modulesUnfiltered = do
  textLn "digraph {"
  indent $ do
    unless splines $ strLn "splines=false;"
    textLn "// Module Clusters"
    forM_ (zip modules [0 :: Int ..]) $ \(mod, i) -> do
      strLn $ "subgraph cluster_" <> show i <> " {"
      indent $ do
        strLn $ "label=" <> show (pmName mod) <> ";"
        printDecls mod
      textLn "}"
    when showCalls $ do
      textLn "// Call graph"
      forM_ calls $ \(caller, callee) -> strLn $ show caller <> " -> " <> show callee <> ";"
  textLn "}"
  where
    modules = include . exclude $ modulesUnfiltered
      where
        include = if null includeFilters then id else filter (matchesAny includeFilters)
        exclude = if null excludeFilters then id else filter (not . matchesAny excludeFilters)
        matchesAny l (ParsedModule name _ _) = any (flip match name) l

    decl :: Int -> Declaration
    decl = (env IM.!)

    calls :: [(Int, Int)]
    calls = IS.toList roots >>= \ix -> collectCalls ix ix
      where
        collectCalls root caller = do
          callee <- IS.toList (declCalls (decl caller))
          let current = (if renderLevel == All then caller else root, callee)
              rest = collectCalls root callee
          if IS.member callee roots then current : rest else rest
        roots = foldMap moduleRoots modules

    printDecls :: ParsedModule -> Printer ()
    printDecls mod@(ParsedModule _ exports binds) = mapM_ printDecl (IS.toList $ moduleRoots mod)
      where
        printDecl ix = do
          let Declaration name _ _ subs _ = decl ix
          strLn $ show ix <> " " <> declStyle ix name <> ";"
          when (renderLevel == All) $
            indent $
              forM_ (IS.toList subs) $ \sub -> do
                strLn $ show ix <> " -> " <> show sub <> " [style=dashed];"
                printDecl sub

        declStyle ix name
          | IS.member ix exports = "[label=" <> name <> ", shape=box]"
          | IS.member ix binds = "[label=" <> name <> ", shape=ellipse]"
          | otherwise = "[label=" <> name <> ", shape=ellipse, style=dashed]"

    moduleRoots :: ParsedModule -> IntSet
    moduleRoots (ParsedModule _ exports binds) = if renderLevel == Exports then exports else binds

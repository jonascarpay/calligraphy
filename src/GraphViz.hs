{-# LANGUAGE OverloadedStrings #-}

module GraphViz where

import Control.Monad
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Lib
import Printer

renderGraphViz :: IntMap Declaration -> [ParsedModule] -> Printer ()
renderGraphViz env modules = do
  textLn "digraph {"
  indent $
    forM_ (zip modules [0 ..]) $ \(ParsedModule modName exports binds, i) -> do
      strLn $ "subgraph cluster_" <> show i <> " {"
      indent $ do
        strLn $ "label=" <> show modName <> ";"
        forM_ (IS.toList binds) $ renderDecl env exports
      textLn "}"
  textLn "}"

renderDecl :: IntMap Declaration -> IntSet -> Int -> Printer ()
renderDecl env exports = go
  where
    go binder = do
      let Declaration name _loc _scope sub calls = env IM.! binder
      strLn $
        show binder
          <> " [label="
          <> show name
          <> (if IS.member binder exports then ", shape=diamond" else mempty)
          <> "];"
      forM_ (IS.toList calls) $ \callee ->
        when (IM.member callee env) $ strLn $ show binder <> " -> " <> show callee <> ";"
      indent $ forM_ (IS.toList sub) go

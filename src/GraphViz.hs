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
  indent $ do
    strLn "splines=false;"
    forM_ (zip modules [0 ..]) $ \(ParsedModule modName exports binds, i) -> do
      strLn $ "subgraph cluster_" <> show i <> " {"
      indent $ do
        strLn $ "label=" <> show modName <> ";"
        forM_ (IS.toList binds) $ renderDecl env exports
      textLn "}"
  textLn "// Call graph"
  indent $
    forM_ modules $ \(ParsedModule modName exports binds) -> do
      forM_ (IS.toList binds) $ renderCall env
  textLn "}"

renderDecl :: IntMap Declaration -> IntSet -> Int -> Printer ()
renderDecl env exports = go
  where
    go binder = do
      let Declaration name _loc _scope subs calls = env IM.! binder
      strLn $
        show binder
          <> " [label="
          <> show name
          <> (if IS.member binder exports then ", shape=diamond" else mempty)
          <> "];"
      forM_ (IS.toList subs) $ \sub -> do
        strLn $ show binder <> " -> " <> show sub <> " [weight=5, style=dashed];"
        indent (go sub)

renderCall :: IntMap Declaration -> Int -> Printer ()
renderCall env = go
  where
    go caller = do
      let Declaration _ _ _ sub calls = env IM.! caller
      forM_ (IS.toList calls) $ \callee -> do
        when (IM.member callee env) $ strLn $ show caller <> " -> " <> show callee <> ";"
      forM_ (IS.toList sub) go

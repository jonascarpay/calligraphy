{-# LANGUAGE OverloadedStrings #-}

module GraphViz where

import Control.Monad
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (intercalate)
import Lib
import Printer

(.=) :: String -> String -> (String, String)
(.=) = (,)

infix 9 .=

showAttrs :: [(String, String)] -> String
showAttrs attrs = "[" <> intercalate ", " (fmap (\(a, b) -> a <> "=" <> b) attrs) <> "]"

renderGraphViz :: IntMap Declaration -> [ParsedModule] -> Printer ()
renderGraphViz env modules = do
  textLn "digraph {"
  indent $ do
    strLn "splines=false;"
    forM_ (zip modules [0 ..]) $ \(ParsedModule modName exports binds, i) -> do
      strLn $ "subgraph cluster_" <> show i <> " {"
      indent $ do
        strLn $ "label=" <> show modName <> ";"
        forM_ (IS.toList binds) $ renderDecl env exports binds
      textLn "}"
  textLn "// Call graph"
  indent $
    forM_ modules $ \(ParsedModule modName exports binds) -> do
      forM_ (IS.toList binds) $ renderCall env
  textLn "}"

renderDecl :: IntMap Declaration -> IntSet -> IntSet -> Int -> Printer ()
renderDecl env exports binds = go
  where
    go binder = do
      let decl = env IM.! binder
          style
            | IS.member binder exports = ["shape" .= "box"]
            | IS.member binder binds = ["shape" .= "ellipse"]
            | otherwise = ["shape" .= "ellipse", "style" .= "dashed"]
      strLn $
        show binder
          <> showAttrs ("label" .= show (declName decl) : style)
          <> ";"
      forM_ (IS.toList (declSub decl)) $ \sub -> do
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

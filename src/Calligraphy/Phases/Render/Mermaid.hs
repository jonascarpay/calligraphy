{-# LANGUAGE OverloadedStrings #-}

module Calligraphy.Phases.Render.Mermaid
  ( renderMermaid,
  )
where

import Calligraphy.Phases.Render.Common
import Calligraphy.Util.Printer
import Control.Monad (forM_)
import Data.List (intercalate)
import Data.Tree (Tree (..))

renderMermaid :: Prints RenderGraph
renderMermaid (RenderGraph roots calls types) = do
  textLn "flowchart TD"
  indent $ do
    case roots of
      Left modules -> mapM_ printModule modules
      Right trees -> mapM_ printTree trees
    forM_ calls $ \(caller, callee) ->
      strLn $ caller <> " --> " <> callee
    forM_ types $ \(caller, callee) ->
      strLn $ caller <> " -.-> " <> callee
  where
    printTree :: Prints (Tree RenderNode)
    printTree (Node (RenderNode nodeid _ lbll _) []) =
      strLn $ nodeid <> "[" <> intercalate "\n" lbll <> "]"
    printTree (Node (RenderNode nodeid _ lbll _) children) =
      brack ("subgraph " <> nodeid <> " [" <> intercalate "\n" lbll <> "]") "end" $
        forM_ children printTree

    printModule :: Prints RenderModule
    printModule (RenderModule lbl modId decls) =
      brack ("subgraph " <> modId <> " [" <> lbl <> "]") "end" $
        forM_ decls printTree

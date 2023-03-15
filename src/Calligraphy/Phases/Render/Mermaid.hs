{-# LANGUAGE OverloadedStrings #-}

module Calligraphy.Phases.Render.Mermaid
  ( renderMermaid,
  )
where

import Calligraphy.Phases.Render.Common
import Calligraphy.Util.Printer
import Calligraphy.Util.Types
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
    printTree (Node (RenderNode nodeid typ lbll export) []) = do
      strLn $ nodeid <> nodeShape typ (intercalate "\n" lbll)
      if export
        then strLn $ "style " <> nodeid <> " stroke:#777,fill-opacity:0"
        else strLn $ "style " <> nodeid <> " stroke:#777,fill-opacity:0,stroke-dasharray: 5 5"
    printTree (Node (RenderNode nodeid typ lbll export) children) = do
      brack ("subgraph " <> nodeid <> "[" <> intercalate "\n" lbll <> "]") "end" $ do
        if export
          then strLn $ "style " <> nodeid <> " stoke:#777,fill-opacity:0"
          else strLn $ "style " <> nodeid <> " stoke:#777,fill-opacity:0,stroke-dasharray: 5 5"
        forM_ children printTree
    -- strLn $ nodeid <> " ~~~ " <> nodeId childNode

    printModule :: Prints RenderModule
    printModule (RenderModule lbl modId decls) =
      brack ("subgraph " <> modId <> " [" <> lbl <> "]") "end" $
        forM_ decls printTree

nodeShape :: DeclType -> String -> String
nodeShape DataDecl = wrapBracket "([" "])"
nodeShape ValueDecl = wrapBracket "[" "]"
nodeShape RecDecl = wrapBracket "(" ")"
nodeShape ConDecl = wrapBracket "(" ")"
nodeShape ClassDecl = wrapBracket "[/" "\\]"

wrapBracket :: String -> String -> String -> String
wrapBracket p q inner = p <> inner <> q

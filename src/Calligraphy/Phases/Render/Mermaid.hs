{-# LANGUAGE OverloadedStrings #-}

module Calligraphy.Phases.Render.Mermaid
  ( renderMermaid,
  )
where

import Calligraphy.Phases.Render.Common
import Calligraphy.Util.Printer
import Calligraphy.Util.Types
import Control.Monad.State (State, execState, modify)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree (..))

renderMermaid :: Prints RenderGraph
renderMermaid (RenderGraph roots calls types) = do
  textLn "flowchart TD"
  indent $ do
    case roots of
      Left modules -> mapM_ printModule modules
      Right trees -> mapM_ printTree trees
    forM_ (removeZeroEdges calls) $ \(caller, callee) ->
      strLn $ caller <> " --> " <> callee
    forM_ (removeZeroEdges types) $ \(caller, callee) ->
      strLn $ caller <> " -.-> " <> callee
    strLn "classDef default fill-opacity:0,stroke:#777;"
  where
    printTree :: Prints (Tree RenderNode)
    printTree (Node (RenderNode nodeid typ lbll export) []) = do
      strLn $ nodeid <> nodeShape typ (intercalate "\\n" lbll)
      unless export . strLn $
        "style " <> nodeid <> " stroke-dasharray: 5 5"
    printTree (Node (RenderNode nodeid typ lbll export) children) = do
      brack ("subgraph " <> nodeid <> "[" <> intercalate "\\n" lbll <> "]") "end" $ do
        unless export . strLn $
          "style " <> nodeid <> " stroke-dasharray: 5 5"
        forM_ children printTree
    -- strLn $ nodeid <> " ~~~ " <> nodeId childNode

    printModule :: Prints RenderModule
    printModule (RenderModule lbl modId decls) =
      brack ("subgraph " <> modId <> " [" <> lbl <> "]") "end" $
        forM_ decls printTree

    -- Because we render hierarchies using subgraphs, there's an odd edge case
    -- when theres an edge between a parent and child; mermaid renders these as
    -- a zero-length edge, i.e. _just_ an arrowhead. So, we remove them.
    removeZeroEdges :: Set (ID, ID) -> Set (ID, ID)
    removeZeroEdges = execState (mapM_ go declRoots)
      where
        declRoots :: NonEmpty (Tree RenderNode)
        declRoots = case roots of
          Left mods -> mods >>= moduleDecls
          Right a -> a
        go :: Tree RenderNode -> State (Set (ID, ID)) ()
        go (Node (RenderNode parentId _ _ _) children) = do
          forM_ children $ \child@(Node (RenderNode childId _ _ _) _) -> do
            modify $ Set.delete (parentId, childId)
            go child

nodeShape :: DeclType -> String -> String
nodeShape DataDecl = wrapBracket "([" "])"
nodeShape ValueDecl = wrapBracket "[" "]"
nodeShape RecDecl = wrapBracket "(" ")"
nodeShape ConDecl = wrapBracket "(" ")"
nodeShape ClassDecl = wrapBracket "[/" "\\]"

wrapBracket :: String -> String -> String -> String
wrapBracket p q inner = p <> inner <> q

{-# LANGUAGE LambdaCase #-}

module Parse where

import Control.Monad.Except
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import GHC qualified
import HieTypes qualified as GHC
import SrcLoc qualified as GHC

-- -- TODO this can be much more efficient. Maybe do something with TangleT?
-- resolve :: Array GHC.TypeIndex GHC.HieTypeFlat -> Array GHC.TypeIndex [Key]
-- resolve arr = imap (\i -> evalState (resolve1 i) mempty) arr
--   where
--     imap :: (GHC.TypeIndex -> b) -> Array GHC.TypeIndex a -> Array GHC.TypeIndex b
--     imap f aa = Array.array (Array.bounds aa) ((\i -> (i, f i)) <$> Array.indices aa)
--     resolve1 :: GHC.TypeIndex -> State (Set GHC.TypeIndex) [Key]
--     resolve1 current = do
--       gets (Set.member current) >>= \case
--         True -> pure []
--         False -> do
--           modify (Set.insert current)
--           case arr Array.! current of
--             GHC.HTyVarTy name -> pure [nameKey name]
--             a -> fold <$> traverse resolve1 a

data DeclType
  = ValueDecl
  | RecDecl
  | ConDecl
  | DataDecl
  | ClassDecl
  deriving
    (Eq, Ord, Show)

data Decl = Decl
  { declType :: DeclType,
    declSpan :: GHC.Span,
    declName :: GHC.Name
  }

data Collect = Collect
  { collectedDecls :: [Decl],
    collectedUses :: [(GHC.RealSrcLoc, GHC.Name)]
  }

data CollectError = UnhandledIdentifier GHC.Name GHC.Span [GHC.ContextInfo]

newtype STree p a = STree (Map p (a, p))

data Node p a = Node
  { nodeLeft :: p,
    nodeVal :: a,
    nodeSub :: STree p a,
    nodeRight :: p
  }

-- insert :: Ord p => p -> a -> p -> STree p a -> STree p a
-- insert l a r = go
--   where
--     go (STree ns) = undefined
--       where
--         (befores, ns1) = Map.split l
--         (parents, ns2) = break ((< l) . nodeLeft) ns1
--         (children, ns3) = break ((< l) . nodeLeft) ns2

-- partitionNodes :: [(p, a, STree p a, p)] -> ([(p, a, STree p a, p)], [(p, a, STree p a, p)], [(p, a, STree p a, p)], [(p, a, STree p a, p)])
-- partitionNodes = undefined

-- go (Node lt lb val sub rb rt) = case (compare l lb, compare r rb) of
--   (LT, GT) -> undefined
--   (GT, LT) -> Node lt lb val (go sub) rb rt
--   _ -> undefined

-- split :: p -> STree p a -> (STree p a, STree p a)
-- split p = go
--   where
--     go Empty = undefined
--     go (Node lt lb v s rb rt) = undefined

structure :: [Decl] -> STree GHC.RealSrcLoc Decl
structure = undefined

collect :: GHC.HieFile -> Either CollectError Collect
collect (GHC.HieFile _ _ _ (GHC.HieASTs asts) _ _) = execStateT (mapM_ collect' asts) (Collect mempty mempty)
  where
    tellDecl :: Decl -> StateT Collect (Either CollectError) ()
    tellDecl decl = modify $ \(Collect decls uses) -> Collect (decl : decls) uses

    tellUse :: GHC.RealSrcLoc -> GHC.Name -> StateT Collect (Either CollectError) ()
    tellUse loc name = modify $ \(Collect decls uses) -> Collect decls ((loc, name) : uses)

    collect' :: GHC.HieAST a -> StateT Collect (Either CollectError) ()
    collect' (GHC.Node (GHC.NodeInfo anns _ ids) nodeSpan children) = do
      forM_ (M.toList ids) $ \case
        (Right name, GHC.IdentifierDetails _ info) ->
          classifyIdentifier
            info
            (\ty sp -> tellDecl $ Decl ty sp name)
            (tellUse (GHC.realSrcSpanStart nodeSpan) name)
            (pure ())
            (throwError $ UnhandledIdentifier name nodeSpan (Set.toList info))
        _ -> pure ()
      mapM_ collect' children

    classifyIdentifier :: Set GHC.ContextInfo -> (DeclType -> GHC.Span -> r) -> r -> r -> r -> r
    classifyIdentifier ctx decl use ignore unknown = case Set.toAscList ctx of
      [GHC.Decl GHC.DataDec (Just sp)] -> decl DataDecl sp
      -- [GHC.Decl GHC.PatSynDec _] -> decl DataDecl
      -- [GHC.Decl GHC.FamDec _] -> decl DataDecl
      -- [GHC.Decl GHC.SynDec _] -> decl DataDecl
      -- [GHC.ClassTyDecl _] -> decl ValueDecl
      -- [GHC.MatchBind, GHC.ValBind _ _ _] -> decl ValueDecl
      -- [GHC.MatchBind] -> decl ValueDecl
      -- [GHC.Decl GHC.InstDec _] -> ignore
      -- [GHC.Decl GHC.ConDec _] -> decl ConDecl
      -- [GHC.Use] -> use
      -- [GHC.Use, GHC.RecField GHC.RecFieldOcc _] -> use
      -- [GHC.Decl GHC.ClassDec _] -> decl ClassDecl
      -- [GHC.ValBind GHC.RegularBind GHC.ModuleScope _, GHC.RecField GHC.RecFieldDecl _] -> decl RecDecl
      -- -- Recordfields without valbind occur when a record occurs in multiple constructors
      -- [GHC.RecField GHC.RecFieldDecl _] -> decl RecDecl
      -- [GHC.PatternBind _ _ _] -> ignore
      -- [GHC.RecField GHC.RecFieldMatch _] -> ignore
      -- [GHC.RecField GHC.RecFieldAssign _] -> use
      -- [GHC.TyDecl] -> ignore
      -- [GHC.IEThing _] -> ignore
      -- [GHC.TyVarBind _ _] -> ignore
      -- -- An empty ValBind is the result of a derived instance, and should be ignored
      -- [GHC.ValBind GHC.RegularBind GHC.ModuleScope _] -> ignore
      _ -> unknown

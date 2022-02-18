{-# LANGUAGE LambdaCase #-}

module Parse
  ( parseHieFile,
    Module (..),
    DeclType (..),
    ParseError (..),
    unKey,
  )
where

import Avail qualified as GHC
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC qualified
import HieTypes qualified as GHC
import Name qualified as GHC
import STree (STree, TreeError)
import STree qualified as ST
import SrcLoc qualified as GHC
import Unique qualified as GHC

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

data ParseError
  = UnhandledIdentifier GHC.Name GHC.Span [GHC.ContextInfo]
  | LexicalError TreeError

data Node p a = Node
  { nodeLeft :: p,
    nodeVal :: a,
    nodeSub :: STree p a,
    nodeRight :: p
  }

data Module = Module
  { modName :: String,
    modExports :: Set Int,
    modTree :: STree GHC.RealSrcLoc (DeclType, GHC.Name),
    modCalls :: Set (Int, Int)
  }

unKey :: GHC.Name -> Int
unKey = GHC.getKey . GHC.nameUnique

parseHieFile :: GHC.HieFile -> Either ParseError Module
parseHieFile file@(GHC.HieFile _ mdl _ asts avails _) = do
  Collect decls uses <- collect asts
  tree <- first LexicalError $ structure decls
  let uses' = Set.fromList $
        flip mapMaybe uses $ \(loc, callee) -> do
          (_, caller) <- ST.lookupInner loc tree
          pure (unKey caller, unKey callee)
      modString = GHC.moduleNameString $ GHC.moduleName mdl
      exportNames = avails >>= GHC.availNames
  pure $ Module modString (Set.fromList $ unKey <$> exportNames) tree uses'

structure :: [Decl] -> Either TreeError (STree GHC.RealSrcLoc (DeclType, GHC.Name))
structure = foldM (\t (Decl ty sp na) -> ST.insert (GHC.realSrcSpanStart sp) (ty, na) (GHC.realSrcSpanEnd sp) t) ST.emptySTree

collect :: GHC.HieASTs a -> Either ParseError Collect
collect (GHC.HieASTs asts) = execStateT (mapM_ collect' asts) (Collect mempty mempty)
  where
    tellDecl :: Decl -> StateT Collect (Either ParseError) ()
    tellDecl decl = modify $ \(Collect decls uses) -> Collect (decl : decls) uses

    tellUse :: GHC.RealSrcLoc -> GHC.Name -> StateT Collect (Either ParseError) ()
    tellUse loc name = modify $ \(Collect decls uses) -> Collect decls ((loc, name) : uses)

    collect' :: GHC.HieAST a -> StateT Collect (Either ParseError) ()
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

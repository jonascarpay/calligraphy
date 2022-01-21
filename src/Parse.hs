{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Parse
  ( foldFile,
    DeclTree (..),
    --  For Debugging
    FoldHead (..),
    Name (..),
    Key (..),
    FoldError (..),
    IdentifierError (..),
    Scope,
    unScope,
  )
where

import Control.Monad.State
import Data.Bifunctor (first)
import Data.Foldable
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC qualified
import GHC.Arr (Array)
import GHC.Arr qualified as Array
import HieTypes qualified as GHC
import Name qualified as GHC
import Unique qualified as GHC

-- TODO this can be much more efficient. Maybe do something with TangleT?
resolve :: Array GHC.TypeIndex GHC.HieTypeFlat -> Array GHC.TypeIndex [Key]
resolve arr = imap (\i -> evalState (resolve1 i) mempty) arr
  where
    imap :: (GHC.TypeIndex -> b) -> Array GHC.TypeIndex a -> Array GHC.TypeIndex b
    imap f aa = Array.array (Array.bounds aa) ((\i -> (i, f i)) <$> Array.indices aa)
    resolve1 :: GHC.TypeIndex -> State (Set GHC.TypeIndex) [Key]
    resolve1 current = do
      gets (Set.member current) >>= \case
        True -> pure []
        False -> do
          modify (Set.insert current)
          case arr Array.! current of
            GHC.HTyVarTy name -> pure [nameKey name]
            a -> fold <$> traverse resolve1 a

newtype Key = Key Int
  deriving (Eq, Ord)

data DeclType
  = ValueDecl
  | RecDecl
  | ConDecl
  | DataDecl
  | ClassDecl
  deriving
    (Eq, Ord, Show)

newtype Scope = Scope (IntMap DeclTree)

instance Semigroup Scope where
  Scope l <> Scope r = Scope $ IM.unionWith f l r
    where
      f (DeclTree t n u c) (DeclTree _ _ u' c') = DeclTree t n (u <> u') (c <> c')

instance Monoid Scope where mempty = Scope mempty

single :: DeclTree -> Scope
single decl@(DeclTree _ (Name (Key key) _) _ _) = Scope $ IM.singleton key decl

unScope :: Scope -> [DeclTree]
unScope (Scope sc) = toList sc

data FoldHead = FoldHead
  { fhDepth :: Int,
    fhDeclType :: DeclType,
    fhDefs :: Map Name (Use, Scope)
  }

data DeclTree = DeclTree
  { declType :: DeclType,
    declName :: Name,
    declUse :: Use,
    children :: Scope
  }

collectMaxima :: forall a b. Ord b => (a -> b) -> NonEmpty a -> (b, NonEmpty a, [a])
collectMaxima ord (a :| as) = foldr f (ord a, pure a, []) as
  where
    f :: a -> (b, NonEmpty a, [a]) -> (b, NonEmpty a, [a])
    f a (b, his, los) = case compare b' b of
      LT -> (b, his, a : los)
      EQ -> (b, NE.cons a his, los)
      GT -> (b', pure a, toList his <> los)
      where
        b' = ord a

foldFile :: GHC.HieFile -> Either FoldError [DeclTree]
foldFile (GHC.HieFile _path _module _types (GHC.HieASTs asts) _info _src) =
  case toList asts of
    [GHC.Node _ _ asts'] -> do
      heads <- traverse foldNode asts'
      pure $ heads >>= toList . fst >>= toDeclTree
    _ -> Left StructuralError

toDeclTree :: FoldHead -> [DeclTree]
toDeclTree (FoldHead _ typ defs) = flip fmap (Map.toList defs) $ \(name, (use, chil)) -> DeclTree typ name use chil

foldHeads :: [(Maybe FoldHead, Use)] -> Either FoldError (Maybe FoldHead, Use)
foldHeads fhs =
  case heads of
    [] -> pure (Nothing, uses)
    (fh : fhs) ->
      case collectMaxima f (fh :| fhs) of
        ((typ, dep), FoldHead _ _ defs :| [], children)
          | [(headName, (headUse, headChil))] <- Map.toList defs ->
            pure
              ( pure $
                  FoldHead (dep - 1) typ $
                    Map.singleton
                      headName
                      (headUse <> uses, foldMap single (children >>= toDeclTree) <> headChil),
                mempty
              )
        ((typ, dep), maxes, []) ->
          pure (pure $ FoldHead (dep - 1) typ (collect maxes), uses)
        _ -> Left (NoFold (fh :| fhs))
  where
    collect :: NonEmpty FoldHead -> Map Name (Use, Scope)
    collect heads = Map.fromListWith mappend (toList heads >>= Map.toList . fhDefs)
    (heads, uses) = foldMap (first toList) fhs
    f :: FoldHead -> (DeclType, Int)
    f (FoldHead d t _) = (t, negate d)

foldNode :: GHC.HieAST a -> Either FoldError (Maybe FoldHead, Use)
foldNode (GHC.Node (GHC.NodeInfo anns _ ids) span children) =
  if isInstanceNode
    then pure (Nothing, mempty)
    else do
      ns <- forM (M.toList ids) $ \case
        (Right name, GHC.IdentifierDetails _ info) ->
          classifyIdentifier
            info
            (\decl -> pure (Just $ FoldHead 0 decl (Map.singleton (unname name) (mempty, mempty)), mempty))
            (pure (Nothing, Use $ Set.singleton (nameKey name)))
            (pure (Nothing, mempty))
            (Left $ IdentifierError span $ UnhandledIdentifier (Right name) info)
        (Left _, _) -> pure (Nothing, mempty)
      subs <- forM children foldNode
      foldHeads (subs <> ns)
  where
    isInstanceNode = Set.member ("ClsInstD", "InstDecl") anns

classifyIdentifier :: Set GHC.ContextInfo -> (DeclType -> r) -> r -> r -> r -> r
classifyIdentifier ctx decl use ignore unknown = case Set.toAscList ctx of
  [GHC.Decl GHC.DataDec _] -> decl DataDecl
  [GHC.Decl GHC.PatSynDec _] -> decl DataDecl
  [GHC.Decl GHC.FamDec _] -> decl DataDecl
  [GHC.Decl GHC.SynDec _] -> decl DataDecl
  [GHC.ClassTyDecl _] -> decl ValueDecl
  [GHC.MatchBind, GHC.ValBind _ _ _] -> decl ValueDecl
  [GHC.MatchBind] -> decl ValueDecl
  [GHC.Decl GHC.InstDec _] -> ignore
  [GHC.Decl GHC.ConDec _] -> decl ConDecl
  [GHC.Use] -> use
  [GHC.Use, GHC.RecField GHC.RecFieldOcc _] -> use
  [GHC.Decl GHC.ClassDec _] -> decl ClassDecl
  [GHC.ValBind GHC.RegularBind GHC.ModuleScope _, GHC.RecField GHC.RecFieldDecl _] -> decl RecDecl
  -- Recordfields without valbind occur when a record occurs in multiple constructors
  [GHC.RecField GHC.RecFieldDecl _] -> decl RecDecl
  [GHC.PatternBind _ _ _] -> ignore
  [GHC.RecField GHC.RecFieldMatch _] -> ignore
  [GHC.RecField GHC.RecFieldAssign _] -> use
  [GHC.TyDecl] -> ignore
  [GHC.IEThing _] -> ignore
  [GHC.TyVarBind _ _] -> ignore
  -- An empty ValBind is the result of a derived instance, and should be ignored
  [GHC.ValBind GHC.RegularBind GHC.ModuleScope _] -> ignore
  _ -> unknown

data FoldError
  = IdentifierError GHC.Span IdentifierError
  | StructuralError
  | NoFold (NonEmpty FoldHead)

data IdentifierError
  = UnhandledIdentifier GHC.Identifier (Set GHC.ContextInfo)

data Name = Name Key String
  deriving (Eq, Ord)

instance Show Name where
  show (Name _ name) = name

newtype Use = Use (Set Key)
  deriving (Eq, Ord, Semigroup, Monoid)

instance Show Use where show _ = "<uses>"

unname :: GHC.Name -> Name
unname n = Name (nameKey n) (GHC.occNameString $ GHC.nameOccName n)

nameKey :: GHC.Name -> Key
nameKey = Key . GHC.getKey . GHC.nameUnique

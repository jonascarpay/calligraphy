{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Parse
  ( parseModule,
    DeclTree (..),
    Module (..),
    DeclType (..),
    --  For Debugging
    Name (..),
    Use (..),
    Key,
    FoldError (..),
    IdentifierError (..),
    Scope,
    unScope,
  )
where

import Avail qualified as GHC
import Control.Monad.State
import Data.Foldable
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import FieldLabel qualified as GHC
import GHC qualified
import GHC.Arr (Array)
import GHC.Arr qualified as Array
import HieTypes qualified as GHC
import Module qualified as GHC
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

type Key = Int

data DeclType
  = ValueDecl
  | RecDecl
  | ConDecl
  | DataDecl
  | ClassDecl
  deriving
    (Eq, Ord, Show)

newtype Scope = Scope (IntMap DeclTree)
  deriving (Eq, Show)

instance Semigroup Scope where
  Scope l <> Scope r = Scope $ IM.unionWith f l r
    where
      f (DeclTree t n u c) (DeclTree _ _ u' c') = DeclTree t n (u <> u') (c <> c')

instance Monoid Scope where mempty = Scope mempty

single :: DeclTree -> Scope
single decl@(DeclTree _ (Name key _) _ _) = Scope $ IM.singleton key decl

unScope :: Scope -> [DeclTree]
unScope (Scope sc) = toList sc

data DeclTree = DeclTree
  { declType :: DeclType,
    declName :: Name,
    declUse :: Use,
    children :: Scope
  }
  deriving (Eq, Show)

data Module = Module
  { modName :: String,
    modExports :: IntSet,
    modNodes :: [DeclTree]
  }

parseModule :: GHC.HieFile -> Either FoldError Module
parseModule (GHC.HieFile _path (GHC.Module _ modname) _types (GHC.HieASTs asts) exports _src) =
  case toList asts of
    [GHC.Node _ _ asts'] -> do
      heads <- traverse foldNode asts'
      let decls =
            heads >>= \(mdom, _) -> case mdom of
              Nothing -> []
              Just (Dominator _ _ r) -> either pure unScope r
      pure $ Module (GHC.moduleNameString modname) (foldMap collect exports) decls
    _ -> Left StructuralError
  where
    collect :: GHC.AvailInfo -> IntSet
    collect (GHC.Avail nm) = IS.singleton (nameKey nm)
    collect (GHC.AvailTC nm extra recs) = IS.fromList . fmap nameKey $ nm : extra <> fmap GHC.flSelector recs

data Dominator = Dominator DeclType Int (Either DeclTree Scope)

combine :: Dominator -> Dominator -> Dominator
combine (Dominator typ dep dec) (Dominator typ' dep' dec') = case compare (typ, negate dep) (typ', negate dep') of
  LT -> Dominator typ' dep' $ assimilate dec' dec
  GT -> Dominator typ dep $ assimilate dec dec'
  EQ -> Dominator typ dep . unsingleton $ merge dec dec'
  where
    unsingleton :: Scope -> Either DeclTree Scope
    unsingleton sc = case unScope sc of
      [decl] -> Left decl
      _ -> Right sc
    forceScope :: Either DeclTree Scope -> Scope
    forceScope = either single id
    assimilate (Left (DeclTree typ name use sub)) sub' = Left $ DeclTree typ name use (forceScope sub' <> sub)
    assimilate (Right sc) dec = Right (sc <> forceScope dec)
    merge l r = forceScope l <> forceScope r

combine' :: Maybe Dominator -> Maybe Dominator -> Maybe Dominator
combine' Nothing a = a
combine' a Nothing = a
combine' (Just a) (Just b) = Just $ combine a b

foldHeads :: [(Maybe Dominator, Use)] -> (Maybe Dominator, Use)
foldHeads fhs =
  case foldr combine' Nothing doms of
    Nothing -> (Nothing, uses)
    Just (Dominator typ dep (Left (DeclTree _ name use sub))) ->
      (Just $ Dominator typ (dep + 1) $ Left $ DeclTree typ name (use <> uses) sub, mempty)
    Just (Dominator typ dep (Right scope)) ->
      (Just $ Dominator typ (dep + 1) $ Right scope, uses)
  where
    (doms, uses) = mconcat <$> unzip fhs

foldNode :: GHC.HieAST a -> Either FoldError (Maybe Dominator, Use)
foldNode (GHC.Node (GHC.NodeInfo anns _ ids) span children) =
  if isInstanceNode
    then pure (Nothing, mempty)
    else do
      ns <- forM (M.toList ids) $ \case
        (Right name, GHC.IdentifierDetails _ info) ->
          classifyIdentifier
            info
            (\decl -> pure (Just $ Dominator decl 0 $ Left $ DeclTree decl (unname name) mempty mempty, mempty))
            (pure (Nothing, Use $ IS.singleton (nameKey name)))
            (pure (Nothing, mempty))
            (Left $ IdentifierError span $ UnhandledIdentifier (Right name) info)
        (Left _, _) -> pure (Nothing, mempty)
      subs <- forM children foldNode
      pure $ foldHeads (subs <> ns)
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

data IdentifierError
  = UnhandledIdentifier GHC.Identifier (Set GHC.ContextInfo)

data Name = Name Key String
  deriving (Eq, Ord)

instance Show Name where
  show (Name _ name) = name

newtype Use = Use IntSet
  deriving (Eq, Ord, Semigroup, Monoid)

instance Show Use where show _ = "<uses>"

unname :: GHC.Name -> Name
unname n = Name (nameKey n) (GHC.getOccString n)

nameKey :: GHC.Name -> Key
nameKey = GHC.getKey . GHC.nameUnique

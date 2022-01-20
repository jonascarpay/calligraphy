{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Parse where

import Control.Monad.State
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import FastString qualified as GHC
import GHC qualified
import GHC.Arr (Array)
import GHC.Arr qualified as Array
import HieTypes qualified as GHC
import Name qualified as GHC
import Pattern
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

data FoldHead = FoldHead
  { fhDepth :: Int,
    fhDeclType :: DeclType,
    fhDefs :: NonEmpty (Name, Use, [DeclTree])
  }

type E a b = forall r. (a -> r) -> (b -> r) -> r

partitionBy :: (a -> Either l r) -> [a] -> ([l], [r])
partitionBy f = partitionEithers . fmap f

data DeclTree = DeclTree
  { declType :: DeclType,
    declName :: Name,
    declUse :: Use,
    children :: [DeclTree]
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
toDeclTree (FoldHead _ typ defs) = flip fmap (toList defs) $ \(name, use, chil) -> DeclTree typ name use chil

foldHeads :: [(Maybe FoldHead, Use)] -> Either FoldError (Maybe FoldHead, Use)
foldHeads fhs =
  case heads of
    [] -> pure (Nothing, uses)
    (fh : fhs) -> case collectMaxima f (fh :| fhs) of
      ((typ, dep), FoldHead _ _ ((headName, headUse, headChil) :| []) :| [], children) ->
        pure
          ( pure $ FoldHead (dep - 1) typ ((headName, headUse <> uses, (children >>= toDeclTree) <> headChil) :| []),
            mempty
          )
      ((typ, dep), maxes, []) ->
        pure
          ( pure $ FoldHead (dep - 1) typ (maxes >>= fhDefs),
            uses
          )
      _ -> Left (NoFold (fh :| fhs))
  where
    (heads, uses) = foldMap (first toList) fhs
    f :: FoldHead -> (DeclType, Int)
    f (FoldHead d t _) = (t, negate d)

foldNode :: GHC.HieAST a -> Either FoldError (Maybe FoldHead, Use)
foldNode (GHC.Node (GHC.NodeInfo anns _ ids) span children) =
  if Set.member ("ClsInstD", "InstDecl") anns
    then pure (Nothing, mempty)
    else do
      ns <- forM (M.toList ids) $ \case
        (Right name, GHC.IdentifierDetails _ info) ->
          classifyIdentifier
            info
            (\decl -> pure (Just $ FoldHead 0 decl (pure (unname name, mempty, mempty)), mempty))
            (pure (Nothing, Use $ Set.singleton (nameKey name)))
            (pure (Nothing, mempty))
            (Left $ IdentifierError span $ UnhandledIdentifier (Right name) info)
        (Left _, _) -> pure (Nothing, mempty)
      subs <- forM children foldNode
      foldHeads (subs <> ns)

isInstance :: GHC.HieAST a -> Bool
isInstance (GHC.Node (GHC.NodeInfo anns _ _) _ _) =
  Set.member ("ClsInstD", "InstDecl") anns

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

data FoldNode
  = FNClass Class
  | FNImport String
  | FNData Data
  | FNCon Con
  | FNValue Value
  | FNValues [Value]
  | FNRecs [Field]
  | FNRec Field
  | FNUse Use
  | FNEmpty
  deriving (Eq, Ord, Show)

data FoldError
  = IdentifierError GHC.Span IdentifierError
  | AppendError GHC.Span AppendError
  | StructuralError
  | NoFold (NonEmpty FoldHead)

foldAst :: GHC.HieAST a -> Either FoldError FoldNode
foldAst (GHC.Node (GHC.NodeInfo anns _ ids) span children) = do
  cs <- traverse foldAst children
  ns <- first (IdentifierError span) $
    forM (M.toList ids) $ \case
      (Left modname, GHC.IdentifierDetails _ info) ->
        case fromModuleName info of
          Nothing -> Left $ UnhandledIdentifier (Left modname) info
          Just f -> Right $ f (GHC.moduleNameString modname)
      (Right name, GHC.IdentifierDetails _ info) ->
        case fromName info (unname name) of
          Nothing -> Left $ UnhandledIdentifier (Right name) info
          Just r -> Right r
  first (AppendError span) $ foldM (curry (toEither (appendNodes anns))) FNEmpty (ns <> cs)

markScope :: FoldNode -> FoldNode
markScope = id

data IdentifierError
  = UnhandledIdentifier GHC.Identifier (Set GHC.ContextInfo)

fromName :: Set GHC.ContextInfo -> Name -> Maybe FoldNode
fromName ctx name@(Name key _) = case Set.toAscList ctx of
  [GHC.Decl GHC.DataDec _] -> datalike
  [GHC.Decl GHC.PatSynDec _] -> datalike
  [GHC.Decl GHC.FamDec _] -> datalike
  [GHC.Decl GHC.SynDec _] -> datalike
  [GHC.ClassTyDecl _] -> valuelike
  [GHC.MatchBind, GHC.ValBind _ _ _] -> valuelike
  [GHC.MatchBind] -> valuelike
  [GHC.Decl GHC.InstDec _] -> ignore
  [GHC.Decl GHC.ConDec _] -> pure $ FNCon (Con name mempty mempty)
  [GHC.Use] -> uselike
  [GHC.Use, GHC.RecField GHC.RecFieldOcc _] -> uselike
  [GHC.Decl GHC.ClassDec _] -> pure $ FNClass $ Class name mempty
  [GHC.ValBind GHC.RegularBind GHC.ModuleScope _, GHC.RecField GHC.RecFieldDecl _] -> recordlike
  -- Recordfields without valbind occur when a record occurs in multiple constructors
  [GHC.RecField GHC.RecFieldDecl _] -> recordlike
  [GHC.PatternBind _ _ _] -> ignore
  [GHC.RecField GHC.RecFieldMatch _] -> ignore
  [GHC.RecField GHC.RecFieldAssign _] -> uselike
  [GHC.TyDecl] -> ignore
  [GHC.IEThing _] -> ignore
  [GHC.TyVarBind _ _] -> ignore
  -- An empty ValBind is the result of a derived instance, and should be ignored
  [GHC.ValBind GHC.RegularBind GHC.ModuleScope _] -> ignore
  _ -> Nothing
  where
    datalike = pure $ FNData $ Data name mempty mempty
    recordlike = pure $ FNRec $ Field name mempty
    valuelike = pure $ FNValue $ Value name mempty mempty
    uselike = pure $ FNUse $ Use $ Set.singleton key
    ignore = pure FNEmpty

fromModuleName :: Set GHC.ContextInfo -> Maybe (String -> FoldNode)
fromModuleName ctx = case Set.toAscList ctx of
  [GHC.IEThing GHC.Import] -> pure FNImport
  [GHC.IEThing GHC.ImportAs] -> pure $ const FNEmpty
  _ -> Nothing

type Annotations = Set (GHC.FastString, GHC.FastString)

data AppendError
  = CombineError Annotations FoldNode FoldNode

appendNodes :: Annotations -> Pattern AppendError (FoldNode, FoldNode) FoldNode
appendNodes anns = generic |> throws (uncurry (CombineError anns))
  where
    generic = fromMaybe $ \case
      (FNEmpty, r) -> pure r
      (l, FNEmpty) -> pure l
      -- (FNUse l, FNUse r) -> pure $ FNUse (l <> r)
      -- (FNCon (Con name use field), FNUse use') -> pure $ FNCon (Con name (use <> use') field)
      -- (FNData (Data name cons use), FNCon con) -> pure $ FNData $ Data name (con : cons) use
      -- (FNRec (Field name use), FNUse use') -> pure $ FNRec $ Field name (use <> use')
      -- (FNRec l, FNRec r) -> pure $ FNRecs [r, l]
      -- (FNRecs recs, FNRec rec) -> pure $ FNRecs (rec : recs)
      -- (l, FNEmpty) -> pure l
      -- (FNUse l, FNUse r) -> pure $ FNUse (l <> r)
      _ -> Nothing
    values = fromMaybe $ \case
      (FNValue l, FNValue r) -> pure $ FNValues [r, l]
      _ -> Nothing
    valuelike = fromMaybe $ \case
      (FNValue (Value name sub use), FNUse use') -> pure $ FNValue $ Value name sub (use' <> use)
      _ -> Nothing
    datalike = fromMaybe $ \case
      (FNData (Data name cons use), FNUse use') -> pure $ FNData $ Data name cons (use' <> use)
      _ -> Nothing
    recordlike = fromMaybe $ \case
      _ -> Nothing
    conlike = fromMaybe $ \case
      (FNCon (Con name use field), FNRecs recs) -> pure $ FNCon (Con name use (recs <> field))
      (FNCon (Con name use field), FNRec rec) -> pure $ FNCon (Con name use (rec : field))
      _ -> Nothing

data Value = Value
  { valName :: Name,
    valChildren :: [Value],
    valUses :: Use
  }
  deriving (Eq, Ord, Show)

data Class = Class
  { clName :: Name,
    clValues :: [Value]
  }
  deriving (Eq, Ord, Show)

data Data = Data
  { dtName :: Name,
    dtCons :: [Con],
    dtUses :: Use
  }
  deriving (Eq, Ord, Show)

data Con = Con
  { conName :: Name,
    conUses :: Use,
    conFields :: [Field]
  }
  deriving (Eq, Ord, Show)

data Field = Field
  { recName :: Name,
    recUses :: Use
  }
  deriving (Eq, Ord, Show)

data Name = Name Key String
  deriving (Eq, Ord)

instance Show Name where
  show (Name _ name) = name

newtype Use = Use (Set Key)
  deriving (Eq, Ord, Semigroup, Monoid)

instance Show Use where show _ = "<uses>"

names :: GHC.HieAST a -> [GHC.Name]
names = (>>= toList) . M.keys . GHC.nodeIdentifiers . GHC.nodeInfo

unname :: GHC.Name -> Name
unname n = Name (nameKey n) (GHC.occNameString $ GHC.nameOccName n)

nameKey :: GHC.Name -> Key
nameKey = Key . GHC.getKey . GHC.nameUnique

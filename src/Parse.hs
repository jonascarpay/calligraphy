{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parse
  ( parseHieFiles,
    Modules (..),
    ModulesDebugInfo (..),
    Name (..),
    Decl (..),
    Key (..),
    Loc (..),
    DeclType (..),
    ParseError (..),
    GHCKey (..),
    unKey,
    rekeyCalls,
    resolveTypes,
  )
where

import qualified Compat as GHC
import Control.Monad.Except
import Control.Monad.State
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Bifunctor
import Data.Bitraversable (bitraverse)
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as EnumSet
import qualified Data.Foldable as Foldable
import Data.List (unzip4)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Forest)
import qualified Data.Tree as Tree
import STree (STree, TreeError)
import qualified STree as ST

-- TODO This can be faster by storing intermediate restuls, but that has proven tricky to get right.
resolveTypes :: Array GHC.TypeIndex GHC.HieTypeFlat -> EnumMap GHC.TypeIndex (EnumSet GHCKey)
resolveTypes typeArray = EnumMap.fromList [(ix, evalState (go ix) mempty) | ix <- Array.indices typeArray]
  where
    keys :: GHC.HieType a -> EnumSet GHCKey
    keys (GHC.HTyConApp (GHC.IfaceTyCon name _) _) = EnumSet.singleton (unKey name)
    keys (GHC.HForAllTy ((name, _), _) _) = EnumSet.singleton (unKey name)
    -- These are variables, which we ignore, but it can't hurt
    keys (GHC.HTyVarTy name) = EnumSet.singleton (unKey name)
    keys _ = mempty
    go ::
      GHC.TypeIndex ->
      State
        (EnumSet GHC.TypeIndex)
        (EnumSet GHCKey)
    go current =
      gets (EnumSet.member current) >>= \case
        True -> pure mempty
        False -> do
          modify (EnumSet.insert current)
          let ty = typeArray Array.! current
          mappend (keys ty) . mconcat <$> mapM go (Foldable.toList ty)

data DeclType
  = ValueDecl
  | RecDecl
  | ConDecl
  | DataDecl
  | ClassDecl
  deriving
    (Eq, Ord, Show)

newtype GHCKey = GHCKey {unGHCKey :: Int}
  deriving newtype (Show, Enum, Eq, Ord)

type GHCDecl = (DeclType, GHC.Span, GHC.Name, Loc)

data Collect = Collect
  { collectedDecls :: [GHCDecl],
    collectedUses :: [(GHC.RealSrcLoc, GHCKey)],
    collectedInferences :: EnumMap GHCKey (EnumSet GHCKey)
  }

data ParseError
  = UnhandledIdentifier GHC.Name GHC.Span [GHC.ContextInfo]
  | TreeError (TreeError GHC.RealSrcLoc (DeclType, Name, Loc))

newtype Key = Key {runKey :: Int}
  deriving (Enum, Show, Eq, Ord)

data Decl = Decl
  { declName :: String,
    declKey :: Key,
    declExported :: Bool,
    declType :: DeclType,
    declLoc :: Loc
  }

data Loc = Loc
  { locLine :: Int,
    locCol :: Int
  }

instance Show Loc where
  showsPrec _ (Loc line col) = shows line . showChar ':' . shows col

data Modules = Modules
  { modules :: [(String, Forest Decl)],
    calls :: Set (Key, Key),
    inferences :: Set (Key, Key)
  }

-- A single symbol can apparently declare a name multiple times in the same place, with multiple distinct keys D:
data Name = Name
  { nameString :: String,
    nameKeys :: EnumSet GHCKey
  }

mkName :: GHC.Name -> Name
mkName nm = Name (GHC.getOccString nm) (EnumSet.singleton $ unKey nm)

-- TODO rename unkey and getkey
unKey :: GHC.Name -> GHCKey
unKey = GHCKey . GHC.getKey . GHC.nameUnique

getKey :: Name -> GHCKey
getKey = EnumSet.findMin . nameKeys

rekeyCalls :: (Enum a, Ord b) => EnumMap a b -> Set (a, a) -> Set (b, b)
rekeyCalls m = foldr (maybe id Set.insert . bitraverse (flip EnumMap.lookup m) (flip EnumMap.lookup m)) mempty

newtype ModulesDebugInfo = ModulesDebugInfo
  { modulesSTrees :: [(String, STree GHC.RealSrcLoc (DeclType, Name, Loc))]
  }

parseHieFiles ::
  [GHC.HieFile] -> Either ParseError (ModulesDebugInfo, Modules)
parseHieFiles files = do
  (parsed, (_, keymap)) <- runStateT (mapM parseFile files) (0, mempty)
  let (mods, debugs, calls, infers) = unzip4 (fmap (\(name, forest, call, infer, stree) -> ((name, forest), (name, stree), call, infer)) parsed)
      inferPairs = rekeyCalls keymap . Set.fromList $ do
        (term, types) <- EnumMap.toList (mconcat infers)
        typ <- EnumSet.toList types
        pure (term, typ)
  pure (ModulesDebugInfo debugs, Modules mods (rekeyCalls keymap (mconcat calls)) inferPairs)
  where
    add :: Key -> Key -> EnumMap Key (EnumSet Key) -> EnumMap Key (EnumSet Key)
    add from to = EnumMap.insertWith (<>) from (EnumSet.singleton to)

    parseFile ::
      GHC.HieFile ->
      StateT
        (Int, EnumMap GHCKey Key)
        (Either ParseError)
        (String, Forest Decl, Set (GHCKey, GHCKey), EnumMap GHCKey (EnumSet GHCKey), STree GHC.RealSrcLoc (DeclType, Name, Loc))
    parseFile file@(GHC.HieFile _ mdl _ _ avails _) = do
      Collect decls uses infers <- lift $ collect file
      tree <- lift $ structure decls
      let calls :: Set (GHCKey, GHCKey) = flip foldMap uses $ \(loc, callee) ->
            case ST.lookupInner loc tree of
              Nothing -> mempty
              Just (_, callerName, _) -> Set.singleton (getKey callerName, callee)
      let exportKeys = EnumSet.fromList $ fmap unKey $ avails >>= GHC.availNames
      forest <- rekey exportKeys (deduplicate tree)
      pure (GHC.moduleNameString (GHC.moduleName mdl), forest, calls, infers, tree)

rekey :: forall m. Monad m => EnumSet GHCKey -> NameTree -> StateT (Int, EnumMap GHCKey Key) m (Forest Decl)
rekey exports = go
  where
    fresh :: StateT (Int, EnumMap GHCKey Key) m Key
    fresh = state $ \(n, m) -> (Key n, (n + 1, m))
    assoc :: Key -> GHCKey -> StateT (Int, EnumMap GHCKey Key) m ()
    assoc key ghckey = modify $ fmap (EnumMap.insert ghckey key)
    go :: NameTree -> StateT (Int, EnumMap GHCKey Key) m (Forest Decl)
    go (NameTree nt) = forM (Map.toList nt) $ \(name, (ghckeys, typ, sub, mloc)) -> do
      key <- fresh
      forM_ (EnumSet.toList ghckeys) (assoc key)
      sub' <- go sub
      let exported = any (flip EnumSet.member exports) (EnumSet.toList ghckeys)
      pure $ Tree.Node (Decl name key exported typ mloc) sub'

newtype NameTree = NameTree (Map String (EnumSet GHCKey, DeclType, NameTree, Loc))

instance Semigroup NameTree where
  NameTree ta <> NameTree tb = NameTree $ Map.unionWith f ta tb
    where
      f (ks, typ, sub, loc) (ks', _, sub', _) = (ks <> ks', typ, sub <> sub', loc)

instance Monoid NameTree where mempty = NameTree mempty

deduplicate :: STree GHC.RealSrcLoc (DeclType, Name, Loc) -> NameTree
deduplicate = ST.foldSTree mempty $ \l _ (typ, Name str ks, mloc) sub _ r ->
  let this = NameTree $ Map.singleton str (ks, typ, sub, mloc)
   in l <> this <> r

structure :: [GHCDecl] -> Either ParseError (STree GHC.RealSrcLoc (DeclType, Name, Loc))
structure =
  foldM
    (\t (ty, sp, na, mloc) -> first TreeError $ ST.insertWith f (GHC.realSrcSpanStart sp) (ty, mkName na, mloc) (GHC.realSrcSpanEnd sp) t)
    ST.emptySTree
  where
    f (ta, Name na ka, mloc) (tb, Name nb kb, _)
      | ta == tb && na == nb = Just (ta, Name na (ka <> kb), mloc)
      | otherwise = Nothing

spanToLoc :: GHC.RealSrcSpan -> Loc
spanToLoc spn = Loc (GHC.srcSpanStartLine spn) (GHC.srcSpanStartCol spn)

collect :: GHC.HieFile -> Either ParseError Collect
collect (GHC.HieFile _ _ typeArr (GHC.HieASTs asts) _ _) = execStateT (mapM_ collect' asts) (Collect mempty mempty mempty)
  where
    tellDecl :: GHCDecl -> StateT Collect (Either ParseError) ()
    tellDecl decl = modify $ \(Collect decls uses infers) -> Collect (decl : decls) uses infers

    tellUse :: GHC.RealSrcLoc -> GHCKey -> StateT Collect (Either ParseError) ()
    tellUse loc key = modify $ \(Collect decls uses infers) -> Collect decls ((loc, key) : uses) infers

    tellInfer :: GHC.Name -> GHC.TypeIndex -> StateT Collect (Either ParseError) ()
    tellInfer name ix = modify $ \(Collect decls uses infers) -> Collect decls uses (EnumMap.insertWith (<>) (unKey name) (typeMap EnumMap.! ix) infers)

    typeMap = resolveTypes typeArr

    collect' :: GHC.HieAST GHC.TypeIndex -> StateT Collect (Either ParseError) ()
    collect' node@(GHC.Node _ nodeSpan children) =
      GHC.forNodeInfos_ node $ \nodeInfo ->
        if GHC.isInstanceNode nodeInfo
          then pure ()
          else do
            forM_ (M.toList $ GHC.nodeIdentifiers nodeInfo) $ \case
              (Right name, GHC.IdentifierDetails mtyp info) -> do
                forM_ mtyp (tellInfer name)
                let decl ty scope = tellDecl (ty, scope, name, spanToLoc nodeSpan)
                GHC.classifyIdentifier
                  info
                  (decl ValueDecl)
                  (decl RecDecl)
                  (decl ConDecl)
                  (decl DataDecl)
                  (decl ClassDecl)
                  (tellUse (GHC.realSrcSpanStart nodeSpan) (unKey name))
                  (pure ())
                  (throwError $ UnhandledIdentifier name nodeSpan (Set.toList info))
              _ -> pure ()
            mapM_ collect' children

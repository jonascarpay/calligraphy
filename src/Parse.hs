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
  )
where

import Avail qualified as GHC
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.Bitraversable (bitraverse)
import Data.EnumMap (EnumMap)
import Data.EnumMap qualified as EnumMap
import Data.EnumSet (EnumSet)
import Data.EnumSet qualified as EnumSet
import Data.Map (Map)
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree (Forest)
import Data.Tree qualified as Tree
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

newtype GHCKey = GHCKey {unGHCKey :: Int}
  deriving newtype (Show, Enum, Eq, Ord)

type GHCDecl = (DeclType, GHC.Span, GHC.Name, Loc)

data Collect = Collect
  { collectedDecls :: [GHCDecl],
    collectedUses :: [(GHC.RealSrcLoc, GHCKey)]
  }

data ParseError
  = UnhandledIdentifier GHC.Name GHC.Span [GHC.ContextInfo]
  | TreeError (TreeError GHC.RealSrcLoc (DeclType, Name, Loc))

newtype Key = Key {runKey :: Int}
  deriving (Enum, Eq, Ord)

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
    calls :: Set (Key, Key)
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
  let (mods, debugs, calls) = unzip3 (fmap (\(name, forest, call, stree) -> ((name, forest), (name, stree), call)) parsed)
  pure (ModulesDebugInfo debugs, Modules mods (rekeyCalls keymap (mconcat calls)))
  where
    add :: Key -> Key -> EnumMap Key (EnumSet Key) -> EnumMap Key (EnumSet Key)
    add from to = EnumMap.insertWith (<>) from (EnumSet.singleton to)

    parseFile ::
      GHC.HieFile ->
      StateT
        (Int, EnumMap GHCKey Key)
        (Either ParseError)
        (String, Forest Decl, Set (GHCKey, GHCKey), STree GHC.RealSrcLoc (DeclType, Name, Loc))
    parseFile (GHC.HieFile _ mdl _ asts avails _) = do
      Collect decls uses <- lift $ collect asts
      tree <- lift $ structure decls
      let calls :: Set (GHCKey, GHCKey) = flip foldMap uses $ \(loc, callee) ->
            case ST.lookupInner loc tree of
              Nothing -> mempty
              Just (_, callerName, _) -> Set.singleton (getKey callerName, callee)
      let exportKeys = EnumSet.fromList $ fmap unKey $ avails >>= GHC.availNames
      forest <- rekey exportKeys (deduplicate tree)
      pure (GHC.moduleNameString (GHC.moduleName mdl), forest, calls, tree)

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

collect :: GHC.HieASTs a -> Either ParseError Collect
collect (GHC.HieASTs asts) = execStateT (mapM_ collect' asts) (Collect mempty mempty)
  where
    tellDecl :: GHCDecl -> StateT Collect (Either ParseError) ()
    tellDecl decl = modify $ \(Collect decls uses) -> Collect (decl : decls) uses

    tellUse :: GHC.RealSrcLoc -> GHCKey -> StateT Collect (Either ParseError) ()
    tellUse loc key = modify $ \(Collect decls uses) -> Collect decls ((loc, key) : uses)

    collect' :: GHC.HieAST a -> StateT Collect (Either ParseError) ()
    collect' (GHC.Node (GHC.NodeInfo anns _ ids) nodeSpan children) =
      if Set.member ("ClsInstD", "InstDecl") anns
        then pure ()
        else do
          forM_ (M.toList ids) $ \case
            (Right name, GHC.IdentifierDetails _ info) ->
              classifyIdentifier
                info
                (\ty scope -> tellDecl (ty, scope, name, spanToLoc nodeSpan))
                (tellUse (GHC.realSrcSpanStart nodeSpan) (unKey name))
                (pure ())
                (throwError $ UnhandledIdentifier name nodeSpan (Set.toList info))
            _ -> pure ()
          mapM_ collect' children

    classifyIdentifier :: Set GHC.ContextInfo -> (DeclType -> GHC.Span -> r) -> r -> r -> r -> r
    classifyIdentifier ctx decl use ignore unknown = case Set.toAscList ctx of
      [GHC.Decl GHC.DataDec (Just sp)] -> decl DataDecl sp
      [GHC.Decl GHC.PatSynDec (Just sp)] -> decl DataDecl sp
      [GHC.Decl GHC.FamDec (Just sp)] -> decl DataDecl sp
      [GHC.Decl GHC.SynDec (Just sp)] -> decl DataDecl sp
      [GHC.ClassTyDecl (Just sp)] -> decl ValueDecl sp
      [GHC.MatchBind, GHC.ValBind _ _ (Just sp)] -> decl ValueDecl sp
      [GHC.MatchBind] -> ignore
      [GHC.Decl GHC.InstDec _] -> ignore
      [GHC.Decl GHC.ConDec (Just sp)] -> decl ConDecl sp
      [GHC.Use] -> use
      [GHC.Use, GHC.RecField GHC.RecFieldOcc _] -> use
      [GHC.Decl GHC.ClassDec (Just sp)] -> decl ClassDecl sp
      [GHC.ValBind GHC.RegularBind GHC.ModuleScope (Just sp), GHC.RecField GHC.RecFieldDecl _] -> decl RecDecl sp
      -- -- Recordfields without valbind occur when a record occurs in multiple constructors
      [GHC.RecField GHC.RecFieldDecl (Just sp)] -> decl RecDecl sp
      [GHC.PatternBind _ _ _] -> ignore
      [GHC.RecField GHC.RecFieldMatch _] -> ignore
      [GHC.RecField GHC.RecFieldAssign _] -> use
      [GHC.TyDecl] -> ignore
      [GHC.IEThing _] -> ignore
      [GHC.TyVarBind _ _] -> ignore
      -- -- An empty ValBind is the result of a derived instance, and should be ignored
      [GHC.ValBind GHC.RegularBind GHC.ModuleScope _] -> ignore
      _ -> unknown

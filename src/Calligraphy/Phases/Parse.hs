{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Calligraphy.Phases.Parse
  ( parseHieFiles,
    -- Debug stuff
    ParseError (..),
    Name (..),
    ModulesDebugInfo (..),
  )
where

import qualified Calligraphy.Compat.GHC as GHC
import Calligraphy.Compat.Lib (classifyIdentifier, forNodeInfos_, isInstanceNode)
import Calligraphy.Util.LexTree (LexTree, TreeError (..))
import qualified Calligraphy.Util.LexTree as LT
import Calligraphy.Util.Types
import Control.Monad.Except
import Control.Monad.State
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Bifunctor (first)
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

newtype GHCKey = GHCKey {_unGHCKey :: Int}
  deriving newtype (Show, Enum, Eq, Ord)

type GHCDecl = (DeclType, GHC.Span, GHC.Name, Loc)

data Collect = Collect
  { _collectedDecls :: [GHCDecl],
    _collectedUses :: [(GHC.RealSrcLoc, GHCKey)],
    _collectedInferences :: EnumMap GHCKey (EnumSet GHCKey)
  }

data ParseError
  = UnhandledIdentifier GHC.Name GHC.Span [GHC.ContextInfo]
  | TreeError (TreeError GHC.RealSrcLoc (DeclType, Name, Loc))

-- A single symbol can apparently declare a name multiple times in the same place, with multiple distinct keys D:
data Name = Name
  { nameString :: String,
    nameKeys :: EnumSet GHCKey
  }
  deriving (Eq, Ord)

mkName :: GHC.Name -> Name
mkName nm = Name (GHC.getOccString nm) (EnumSet.singleton $ unKey nm)

-- TODO rename unkey and getkey
unKey :: GHC.Name -> GHCKey
unKey = GHCKey . GHC.getKey . GHC.nameUnique

getKey :: Name -> GHCKey
getKey = EnumSet.findMin . nameKeys

newtype ModulesDebugInfo = ModulesDebugInfo
  { modulesLexTrees :: [(String, LexTree GHC.RealSrcLoc (DeclType, Name, Loc))]
  }

parseHieFiles ::
  [GHC.HieFile] -> Either ParseError (ModulesDebugInfo, Modules)
parseHieFiles files = do
  (parsed, (_, keymap)) <- runStateT (mapM parseFile files) (0, mempty)
  let (mods, debugs, calls, infers) = unzip4 (fmap (\(name, forest, call, infer, ltree) -> ((name, forest), (name, ltree), call, infer)) parsed)
      inferPairs = rekeyCalls keymap . Set.fromList $ do
        (term, types) <- EnumMap.toList (mconcat infers)
        typ <- EnumSet.toList types
        pure (term, typ)
  pure (ModulesDebugInfo debugs, Modules mods (rekeyCalls keymap (mconcat calls)) inferPairs)
  where
    parseFile ::
      GHC.HieFile ->
      StateT
        (Int, EnumMap GHCKey Key)
        (Either ParseError)
        (String, Forest Decl, Set (GHCKey, GHCKey), EnumMap GHCKey (EnumSet GHCKey), LexTree GHC.RealSrcLoc (DeclType, Name, Loc))
    parseFile file@(GHC.HieFile _ mdl _ _ avails _) = do
      Collect decls uses infers <- lift $ collect file
      tree <- lift $ structure decls
      let calls :: Set (GHCKey, GHCKey) = flip foldMap uses $ \(loc, callee) ->
            case LT.lookup loc tree of
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

deduplicate :: LexTree GHC.RealSrcLoc (DeclType, Name, Loc) -> NameTree
deduplicate = LT.foldLexTree mempty $ \l _ (typ, Name str ks, mloc) sub _ r ->
  let this = NameTree $ Map.singleton str (ks, typ, sub, mloc)
   in l <> this <> r

structure :: [GHCDecl] -> Either ParseError (LexTree GHC.RealSrcLoc (DeclType, Name, Loc))
structure =
  foldM
    (\t (ty, sp, na, mloc) -> first TreeError $ LT.insertWith f (GHC.realSrcSpanStart sp) (ty, mkName na, mloc) (GHC.realSrcSpanEnd sp) t)
    LT.emptyLexTree
  where
    f (ta, Name na ka, mloc) (tb, Name nb kb, _)
      | ta == tb && na == nb = Just (ta, Name na (ka <> kb), mloc)
      | otherwise = Nothing

spanToLoc :: GHC.RealSrcSpan -> Loc
spanToLoc spn = Loc (GHC.srcSpanStartLine spn) (GHC.srcSpanStartCol spn)

data NodeType
  = EmptyNode
  | UseNode GHCKey GHC.RealSrcLoc
  | DeclNode DeclType GHC.Name GHC.Span
  deriving (Eq, Ord)

classifyNode :: GHC.HieAST GHC.TypeIndex -> Either ParseError NodeType
classifyNode node = (\l -> if null l then EmptyNode else maximum l) <$> types
  where
    types :: Either ParseError [NodeType]
    types = flip execStateT [] $
      forNodeInfos_ node $ \nodeInfo ->
        forM_ (M.toList $ GHC.nodeIdentifiers nodeInfo) $ \case
          (Right name, GHC.IdentifierDetails _ info) ->
            let decl :: DeclType -> GHC.Span -> StateT [NodeType] (Either ParseError) ()
                decl ty scope = modify (DeclNode ty name scope :)
             in classifyIdentifier
                  info
                  (decl ValueDecl)
                  (decl RecDecl)
                  (decl ConDecl)
                  (decl DataDecl)
                  (decl ClassDecl)
                  (modify (UseNode (unKey name) (GHC.realSrcSpanStart $ GHC.nodeSpan node) :))
                  (pure ())
                  (throwError $ UnhandledIdentifier name (GHC.nodeSpan node) (Set.toList info))
          _ -> pure ()

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
    collect' node@(GHC.Node _ _ children) =
      forNodeInfos_ node $ \nodeInfo ->
        if isInstanceNode nodeInfo
          then pure ()
          else do
            forM_ (M.toList $ GHC.nodeIdentifiers nodeInfo) $ \case
              (Right name, GHC.IdentifierDetails ty _) -> mapM_ (tellInfer name) ty
              _ -> pure ()
            lift (classifyNode node) >>= \case
              EmptyNode -> pure ()
              UseNode gk rsl -> tellUse rsl gk
              DeclNode dt na rss -> tellDecl (dt, rss, na, spanToLoc rss)
            mapM_ collect' children

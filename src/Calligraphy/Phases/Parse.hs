{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Calligraphy.Phases.Parse
  ( parseHieFiles,
    ppParseError,
    ppParsePhaseDebugInfo,
    ParseError (..),
    ParsePhaseDebugInfo (..),
  )
where

import qualified Calligraphy.Compat.GHC as GHC
import Calligraphy.Compat.Lib (isDerivingNode, isInlineNode, isInstanceNode, isMinimalNode, isTypeSignatureNode, mergeSpans, sourceInfo)
import qualified Calligraphy.Compat.Lib as GHC
import Calligraphy.Util.LexTree (LexTree, TreeError (..), foldLexTree)
import qualified Calligraphy.Util.LexTree as LT
import Calligraphy.Util.Printer
import Calligraphy.Util.Types
import Control.Monad.Except
import Control.Monad.State
import Data.Array (Array)
import qualified Data.Array as Array
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as EnumSet
import qualified Data.Foldable as Foldable
import Data.List (unzip4)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Forest, Tree (..))

-- | A declaration extracted from the source code.
--
-- A single symbol can apparently declare a name multiple times in the same place, with multiple distinct keys D:
-- This happens, for example, with default methods; the name refers to both the method name and the default implementation's name.
-- We have to account for that to _some_ degree, which is why keys is a set.
-- The actual resolution of these happens wit 'dedup' in mkForest
data RawDecl = RawDecl
  { _rdName :: !String,
    rdKeys :: !(EnumSet GHCKey),
    _rdTyp :: !DeclType,
    rdStart :: !Loc,
    rdEnd :: !Loc
  }

data ParseError = TreeError
  { _peModuleName :: String,
    _peModulePath :: FilePath,
    _peError :: TreeError Loc RawDecl
  }

ppParseError :: Prints ParseError
ppParseError (TreeError str path err) = do
  strLn $ "Parse error in module " <> str <> " (" <> path <> ")"
  indent $ ppTreeError err
  where
    ppTreeError :: Prints (TreeError Loc RawDecl)
    ppTreeError (InvalidBounds l decl r) = do
      strLn $ "Invalid bounds " <> show (l, r) <> "while inserting"
      indent $ ppRawDecl decl
    ppTreeError (OverlappingBounds a b l r) = do
      strLn $ "Clashing bounds: (" <> show (l, r) <> ")"
      strLn "Node 1:"
      indent $ ppRawDecl a
      strLn "Node 2:"
      indent $ ppRawDecl b
    ppTreeError MidSplit = strLn "MidSplit"
    ppTreeError (LexicalError l decl r t) = do
      strLn "Lexical error while inserting"
      strLn "Node:"
      indent $ ppRawDecl decl
      strLn "Bounds:"
      indent $ showLn (l, r)
      strLn "Tree:"
      indent $ do
        ppLexTree t

ppRawDecl :: Prints RawDecl
ppRawDecl (RawDecl name keys typ st end) = do
  strLn name
  indent $ do
    strLn $ "Type: " <> show typ
    strLn $ "Span: " <> show (st, end)
    strLn $ "Keys: " <> unwords (show <$> EnumSet.toList keys)

ppLexTree :: Prints (LexTree Loc RawDecl)
ppLexTree = foldLexTree (pure ()) $ \ls l decl m r rs -> do
  ls
  showLn (l, r)
  ppRawDecl decl
  indent m
  rs

ghcNameKey :: GHC.Name -> GHCKey
ghcNameKey = GHCKey . GHC.getKey . GHC.nameUnique

newtype ParsePhaseDebugInfo = ParsePhaseDebugInfo {modulesLexTrees :: [(String, LexTree Loc RawDecl)]}

ppParsePhaseDebugInfo :: Prints ParsePhaseDebugInfo
ppParsePhaseDebugInfo (ParsePhaseDebugInfo mods) = forM_ mods $ \(modName, ltree) -> do
  strLn modName
  indent $ ppLexTree ltree

data ParsedFile = ParsedFile
  { _pfModuleName :: String,
    _pfFilePath :: FilePath,
    _pfDecls :: Forest Decl,
    _pfCalls :: Set (GHCKey, GHCKey),
    _pfTypings :: EnumMap GHCKey (EnumSet GHCKey),
    _pfDebugTree :: LexTree Loc RawDecl
  }

-- | Assigns and maintains a mapping of GHCKeys to Key
type HieParse a = StateT (Key, EnumMap GHCKey Key) (Either ParseError) a

parseHieFiles ::
  [GHC.HieFile] ->
  Either ParseError (ParsePhaseDebugInfo, CallGraph)
parseHieFiles files = (\(parsed, (_, keymap)) -> mkCallGraph parsed keymap) <$> runStateT (mapM parseHieFile files) (Key 0, mempty)
  where
    mkCallGraph :: [ParsedFile] -> EnumMap GHCKey Key -> (ParsePhaseDebugInfo, CallGraph)
    mkCallGraph parsed keymap =
      let (mods, debugs, calls, typings) = unzip4 (fmap (\(ParsedFile name path decls call typing ltree) -> (Module name path decls, (name, ltree), call, typing)) parsed)
          typeEdges = rekeyCalls keymap . Set.fromList $ do
            (term, types) <- EnumMap.toList (mconcat typings)
            typ <- EnumSet.toList types
            pure (term, typ)
       in (ParsePhaseDebugInfo debugs, CallGraph mods (rekeyCalls keymap (mconcat calls)) typeEdges)

parseHieFile :: GHC.HieFile -> HieParse ParsedFile
parseHieFile file@(GHC.HieFile filepath mdl _ _ avails _) = do
  lextree <- either (throwError . TreeError modname filepath) pure $ structure decls
  let calls = resolveCalls (fmap (EnumSet.findMin . rdKeys) lextree)
  forest <- forestT (mkDecl exportKeys) (mkForest lextree)
  pure $ ParsedFile modname filepath forest calls types lextree
  where
    modname = GHC.moduleNameString (GHC.moduleName mdl)
    exportKeys = EnumSet.fromList $ fmap ghcNameKey $ avails >>= GHC.availNames
    Collect decls useSites types = collect file

    resolveCalls :: LexTree Loc GHCKey -> Set (GHCKey, GHCKey)
    resolveCalls lextree = flip foldMap useSites $ \(loc, callee) ->
      case LT.lookup loc lextree of
        Nothing -> mempty
        Just rep -> Set.singleton (rep, callee)

    mkForest :: LexTree Loc RawDecl -> Forest RawDecl
    mkForest = over forestT fromKV . dedup . over forestT toKV . LT.toForest
      where
        toKV (_, RawDecl name keys typ s e, _) = (name, (keys, Max typ, First s, First e))
        fromKV (name, (keys, Max typ, First s, First e)) = RawDecl name keys typ s e

    -- TODO this is the only part that touches the state, maybe it's worth lifting it out
    mkDecl :: EnumSet GHCKey -> RawDecl -> HieParse Decl
    mkDecl exportSet (RawDecl str ghcKeys typ start _) = do
      key <- fresh
      forM_ (EnumSet.toList ghcKeys) (assoc key)
      let exported = any (flip EnumSet.member exportSet) (EnumSet.toList ghcKeys)
      pure $ Decl str key ghcKeys exported typ start

    fresh :: HieParse Key
    fresh = state $ \(Key n, m) -> (Key n, (Key (n + 1), m))

    assoc :: Key -> GHCKey -> HieParse ()
    assoc key ghckey = modify $ fmap (EnumMap.insert ghckey key)

dedup :: (Ord k, Semigroup v) => Forest (k, v) -> Forest (k, v)
dedup = fromDedup . toDedup
  where
    fromDedup = fmap (\(k, (v, d)) -> Node (k, v) (fromDedup d)) . Map.toList . unDedup
    toDedup = Dedup . Map.fromListWith (<>) . fmap (\(Node (k, v) f) -> (k, (v, toDedup f)))

newtype Dedup k v = Dedup {unDedup :: Map k (v, Dedup k v)}

instance (Ord k, Semigroup v) => Semigroup (Dedup k v) where
  Dedup a <> Dedup b = Dedup (Map.unionWith (<>) a b)

structure :: [RawDecl] -> Either (TreeError Loc RawDecl) (LexTree Loc RawDecl)
structure = foldM (\ !t decl -> LT.insertWith f (rdStart decl) decl (rdEnd decl) t) LT.emptyLexTree
  where
    f (RawDecl na ka ta sa ea) prev@(RawDecl nb kb tb _ _)
      | ta == tb && na == nb = Just (RawDecl na (ka <> kb) ta sa ea)
      | otherwise = Just prev

-- | This is the best way I can find of checking whether the name was written by a programmer or not.
-- GHC internally classifies names extensively, but none of those mechanisms seem to allow to distinguish GHC-generated names.
isGenerated :: GHC.Name -> Bool
isGenerated = elem '$' . GHC.getOccString

data Collect = Collect
  { _decls :: [RawDecl],
    _uses :: [(Loc, GHCKey)],
    _types :: EnumMap GHCKey (EnumSet GHCKey)
  }

-- | Collect declarations, uses, and types in a HIE file
collect :: GHC.HieFile -> Collect
collect (GHC.HieFile _ _ typeArr (GHC.HieASTs asts) _ _) = execState (forT_ traverse asts go) (Collect mempty mempty mempty)
  where
    tellDecl :: GHC.Name -> DeclType -> GHC.RealSrcSpan -> State Collect ()
    tellDecl nm typ spn = modify $ \(Collect decls uses types) -> Collect (decl : decls) uses types
      where
        decl =
          RawDecl
            (GHC.getOccString nm)
            (EnumSet.singleton . ghcNameKey $ nm)
            typ
            (Loc (GHC.srcSpanStartLine spn) (GHC.srcSpanStartCol spn))
            (Loc (GHC.srcSpanEndLine spn) (GHC.srcSpanEndCol spn))

    tellUse :: GHC.RealSrcLoc -> GHCKey -> State Collect ()
    tellUse loc key = modify $ \(Collect decls uses types) -> Collect decls ((Loc (GHC.srcLocLine loc) (GHC.srcLocCol loc), key) : uses) types

    tellType :: GHC.Name -> GHC.TypeIndex -> State Collect ()
    tellType name ix = modify $ \(Collect decls uses types) -> Collect decls uses (EnumMap.insertWith (<>) (ghcNameKey name) (typeMap EnumMap.! ix) types)

    typeMap = resolveTypes typeArr

    ignoreNode nodeInfo = any ($ nodeInfo) [isInstanceNode, isTypeSignatureNode, isInlineNode, isMinimalNode, isDerivingNode]

    go :: GHC.HieAST GHC.TypeIndex -> State Collect ()
    go node@(GHC.Node _ _ children) =
      forT_ sourceInfo node $ \nodeInfo ->
        unless (ignoreNode nodeInfo) $ do
          forM_ (M.toList $ GHC.nodeIdentifiers nodeInfo) $ \case
            (Right name, GHC.IdentifierDetails ty info) | not (isGenerated name) -> do
              mapM_ (tellType name) ty
              case classifyIdentifier info of
                IdnIgnore -> pure ()
                IdnUse -> tellUse (GHC.realSrcSpanStart $ GHC.nodeSpan node) (ghcNameKey name)
                IdnDecl typ sp
                  | GHC.isPointSpan sp -> pure ()
                  | otherwise -> tellDecl name typ sp
            _ -> pure ()
          mapM_ go children

-- TODO This can be faster by storing intermediate restuls, but that has proven tricky to get right.
resolveTypes :: Array GHC.TypeIndex GHC.HieTypeFlat -> EnumMap GHC.TypeIndex (EnumSet GHCKey)
resolveTypes typeArray = EnumMap.fromList [(ix, evalState (go ix) mempty) | ix <- Array.indices typeArray]
  where
    keys :: GHC.HieType a -> EnumSet GHCKey
    keys (GHC.HTyConApp (GHC.IfaceTyCon name _) _) = EnumSet.singleton (ghcNameKey name)
    keys (GHC.HForAllTy ((name, _), _) _) = EnumSet.singleton (ghcNameKey name)
    -- These are variables, which we ignore, but it can't hurt
    keys (GHC.HTyVarTy name) = EnumSet.singleton (ghcNameKey name)
    keys _ = mempty
    go :: GHC.TypeIndex -> State (EnumSet GHC.TypeIndex) (EnumSet GHCKey)
    go current =
      gets (EnumSet.member current) >>= \case
        True -> pure mempty
        False -> do
          modify (EnumSet.insert current)
          let ty = typeArray Array.! current
          mappend (keys ty) . mconcat <$> mapM go (Foldable.toList ty)

data IdentifierType
  = IdnDecl !DeclType !GHC.Span
  | IdnUse
  | IdnIgnore

instance Semigroup IdentifierType where
  IdnIgnore <> a = a
  IdnUse <> IdnIgnore = IdnUse
  IdnUse <> a = a
  IdnDecl typ sp <> IdnDecl typ' sp' = IdnDecl (max typ typ') (mergeSpans sp sp')
  IdnDecl typ sp <> _ = IdnDecl typ sp

instance Monoid IdentifierType where mempty = IdnIgnore

classifyIdentifier :: Set GHC.ContextInfo -> IdentifierType
classifyIdentifier = foldMap classify
  where
    classify :: GHC.ContextInfo -> IdentifierType
    classify (GHC.Decl GHC.DataDec (Just sp)) = IdnDecl DataDecl sp
    classify (GHC.Decl GHC.PatSynDec (Just sp)) = IdnDecl DataDecl sp
    classify (GHC.Decl GHC.FamDec (Just sp)) = IdnDecl DataDecl sp
    classify (GHC.Decl GHC.SynDec (Just sp)) = IdnDecl DataDecl sp
    classify (GHC.Decl GHC.ConDec (Just sp)) = IdnDecl ConDecl sp
    classify (GHC.Decl GHC.ClassDec (Just sp)) = IdnDecl ClassDecl sp
    classify (GHC.ClassTyDecl (Just sp)) = IdnDecl ValueDecl sp
    classify (GHC.ValBind GHC.RegularBind _ (Just sp)) = IdnDecl ValueDecl sp
    classify (GHC.RecField GHC.RecFieldDecl (Just sp)) = IdnDecl RecDecl sp
    -- Use
    classify (GHC.RecField GHC.RecFieldAssign _) = IdnUse
    classify (GHC.RecField GHC.RecFieldOcc _) = IdnUse
    classify GHC.Use = IdnUse
    -- Ignore
    classify _ = IdnIgnore

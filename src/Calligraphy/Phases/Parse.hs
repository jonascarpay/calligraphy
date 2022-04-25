{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Calligraphy.Compat.Lib (forNodeInfos_, isDerivingNode, isInlineNode, isInstanceNode, isMinimalNode, isTypeSignatureNode, spanSpans)
import Calligraphy.Util.LexTree (LexTree, TreeError (..), foldLexTree)
import qualified Calligraphy.Util.LexTree as LT
import Calligraphy.Util.Printer
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

type GHCDecl = (DeclType, GHC.Span, GHC.Name, Loc)

data Collect = Collect
  { _decls :: [GHCDecl],
    _uses :: [(GHC.RealSrcLoc, GHCKey)],
    _types :: EnumMap GHCKey (EnumSet GHCKey)
  }

newtype ParseError = TreeError (TreeError GHC.RealSrcLoc (DeclType, Name, Loc))

ppParseError :: Prints ParseError
ppParseError (TreeError err) = ppTreeError err
  where
    ppTreeError :: Prints (TreeError GHC.RealSrcLoc (DeclType, Name, Loc))
    ppTreeError (InvalidBounds l (ty, nm, _) r) = strLn "Invalid bounds:" >> indent (ppLocNode l r ty nm)
    ppTreeError (OverlappingBounds (ty, nm, _) (ty', nm', _) l r) = do
      strLn $ "OverlappingBounds bounds: (" <> show (l, r) <> ")"
      indent $ do
        strLn $ showName nm <> " (" <> show ty <> ")"
        strLn $ showName nm' <> " (" <> show ty' <> ")"
    ppTreeError MidSplit = strLn "MidSplit"
    ppTreeError (LexicalError l (ty, nm, _) r t) = do
      strLn "Lexical error"
      indent $ do
        ppLocNode l r ty nm
        ppLexTree t

showName :: Name -> String
showName (Name name keys) = name <> "    " <> show (EnumSet.toList keys)

ppLocNode :: GHC.RealSrcLoc -> GHC.RealSrcLoc -> DeclType -> Name -> Printer ()
ppLocNode l r typ name = strLn $ showName name <> " (" <> show typ <> ") " <> show l <> " " <> show r

ppLexTree :: Prints (LexTree GHC.RealSrcLoc (DeclType, Name, Loc))
ppLexTree = foldLexTree (pure ()) $ \ls l (typ, name, _loc) m r rs -> do
  ls
  ppLocNode l r typ name
  indent m
  rs

-- A single symbol can apparently declare a name multiple times in the same place, with multiple distinct keys D:
data Name = Name
  { _nameString :: String,
    nameKeys :: EnumSet GHCKey
  }
  deriving (Eq, Ord)

mkName :: GHC.Name -> Name
mkName nm = Name (GHC.getOccString nm) (EnumSet.singleton $ ghcNameKey nm)

ghcNameKey :: GHC.Name -> GHCKey
ghcNameKey = GHCKey . GHC.getKey . GHC.nameUnique

-- TODO rename, clean up
newtype ParsePhaseDebugInfo = ParsePhaseDebugInfo
  { modulesLexTrees :: [(String, LexTree GHC.RealSrcLoc (DeclType, Name, Loc))]
  }

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
    _pfDebugTree :: LexTree GHC.RealSrcLoc (DeclType, Name, Loc)
  }

parseHieFiles ::
  [GHC.HieFile] ->
  Either ParseError (ParsePhaseDebugInfo, CallGraph)
parseHieFiles files = do
  (parsed, (_, keymap)) <- runStateT (mapM parseFile files) (0, mempty)
  let (mods, debugs, calls, typings) = unzip4 (fmap (\(ParsedFile name path forest call typing ltree) -> ((Module name path forest), (name, ltree), call, typing)) parsed)
      typeEdges = rekeyCalls keymap . Set.fromList $ do
        (term, types) <- EnumMap.toList (mconcat typings)
        typ <- EnumSet.toList types
        pure (term, typ)
  pure (ParsePhaseDebugInfo debugs, CallGraph mods (rekeyCalls keymap (mconcat calls)) typeEdges)
  where
    parseFile ::
      GHC.HieFile ->
      StateT
        (Int, EnumMap GHCKey Key)
        (Either ParseError)
        ParsedFile
    parseFile file@(GHC.HieFile filepath mdl _ _ avails _) = do
      Collect decls uses types <- lift $ collect file
      tree <- lift $ structure decls
      let calls :: Set (GHCKey, GHCKey) = flip foldMap uses $ \(loc, callee) ->
            case LT.lookup loc tree of
              Nothing -> mempty
              Just (_, callerName, _) -> Set.singleton (nameKey callerName, callee)
      let exportKeys = EnumSet.fromList $ fmap ghcNameKey $ avails >>= GHC.availNames
      forest <- rekey exportKeys (deduplicate tree)
      pure $ ParsedFile (GHC.moduleNameString (GHC.moduleName mdl)) filepath forest calls types tree
    nameKey :: Name -> GHCKey
    nameKey = EnumSet.findMin . nameKeys

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
      pure $ Tree.Node (Decl name key ghckeys exported typ mloc) sub'

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

-- | This is the best way I can find of checking whether the name was written by a programmer or not.
-- GHC internally classifies names extensively, but none of those mechanisms seem to allow to distinguish GHC-generated names.
isGenerated :: GHC.Name -> Bool
isGenerated = elem '$' . GHC.getOccString

collect :: GHC.HieFile -> Either ParseError Collect
collect (GHC.HieFile _ _ typeArr (GHC.HieASTs asts) _ _) = execStateT (forT_ traverse asts collect') (Collect mempty mempty mempty)
  where
    tellDecl :: GHCDecl -> StateT Collect (Either ParseError) ()
    tellDecl decl = modify $ \(Collect decls uses types) -> Collect (decl : decls) uses types

    tellUse :: GHC.RealSrcLoc -> GHCKey -> StateT Collect (Either ParseError) ()
    tellUse loc key = modify $ \(Collect decls uses types) -> Collect decls ((loc, key) : uses) types

    tellType :: GHC.Name -> GHC.TypeIndex -> StateT Collect (Either ParseError) ()
    tellType name ix = modify $ \(Collect decls uses types) -> Collect decls uses (EnumMap.insertWith (<>) (ghcNameKey name) (typeMap EnumMap.! ix) types)

    typeMap = resolveTypes typeArr

    collect' :: GHC.HieAST GHC.TypeIndex -> StateT Collect (Either ParseError) ()
    collect' node@(GHC.Node _ _ children) =
      forNodeInfos_ node $ \nodeInfo ->
        if any ($ nodeInfo) [isInstanceNode, isTypeSignatureNode, isInlineNode, isMinimalNode, isDerivingNode]
          then pure ()
          else do
            forM_ (M.toList $ GHC.nodeIdentifiers nodeInfo) $ \case
              (Right name, GHC.IdentifierDetails ty info)
                | not (isGenerated name) -> do
                    mapM_ (tellType name) ty
                    case classifyIdentifier info of
                      IdnIgnore -> pure ()
                      IdnUse -> tellUse (GHC.realSrcSpanStart $ GHC.nodeSpan node) (ghcNameKey name)
                      IdnDecl typ sp -> tellDecl (typ, sp, name, spanToLoc sp)
              _ -> pure ()
            mapM_ collect' children

data IdentifierType
  = IdnDecl DeclType GHC.Span
  | IdnUse
  | IdnIgnore

instance Semigroup IdentifierType where
  IdnIgnore <> a = a
  IdnUse <> IdnIgnore = IdnUse
  IdnUse <> a = a
  IdnDecl typ sp <> IdnDecl typ' sp' = IdnDecl (max typ typ') (spanSpans sp sp')
  IdnDecl typ sp <> _ = IdnDecl typ sp

instance Monoid IdentifierType where mempty = IdnIgnore

classifyIdentifier :: Set GHC.ContextInfo -> IdentifierType
classifyIdentifier = foldMap classify
  where
    classify :: GHC.ContextInfo -> IdentifierType
    classify (GHC.Decl GHC.DataDec (Just sp)) = (IdnDecl DataDecl sp)
    classify (GHC.Decl GHC.PatSynDec (Just sp)) = (IdnDecl DataDecl sp)
    classify (GHC.Decl GHC.FamDec (Just sp)) = (IdnDecl DataDecl sp)
    classify (GHC.Decl GHC.SynDec (Just sp)) = (IdnDecl DataDecl sp)
    classify (GHC.Decl GHC.ConDec (Just sp)) = (IdnDecl ConDecl sp)
    classify (GHC.Decl GHC.ClassDec (Just sp)) = (IdnDecl ClassDecl sp)
    classify (GHC.ClassTyDecl (Just sp)) = (IdnDecl ValueDecl sp)
    classify (GHC.ValBind GHC.RegularBind _ (Just sp)) = (IdnDecl ValueDecl sp)
    classify (GHC.RecField GHC.RecFieldDecl (Just sp)) = (IdnDecl RecDecl sp)
    -- Use
    classify (GHC.RecField GHC.RecFieldAssign _) = IdnUse
    classify (GHC.RecField GHC.RecFieldOcc _) = IdnUse
    classify GHC.Use = IdnUse
    -- Ignore
    classify _ = IdnIgnore

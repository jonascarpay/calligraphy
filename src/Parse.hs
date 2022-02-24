{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( parseHieFile,
    Module (..),
    Name (..),
    DeclType (..),
    ParseError (..),
    SemanticTree (..),
    unKey,
  )
where

import Avail qualified as GHC
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map (Map)
import Data.Map qualified as M
import Data.Map qualified as Map
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
    declName :: Name
  }

data Collect = Collect
  { collectedDecls :: [Decl],
    collectedUses :: [(GHC.RealSrcLoc, GHC.Name)]
  }

data ParseError
  = UnhandledIdentifier GHC.Name GHC.Span [GHC.ContextInfo]
  | TreeError (TreeError GHC.RealSrcLoc (DeclType, Name))

data Module = Module
  { modName :: String,
    modExports :: IntSet,
    modTree :: SemanticTree,
    modCalls :: Set (Int, Int)
  }

-- A single symbol can apparently declare a name with multiple distinct keys D:
data Name = Name
  { nameString :: String,
    nameKeys :: IntSet
  }

mkName :: GHC.Name -> Name
mkName nm = Name (GHC.getOccString nm) (IntSet.singleton $ unKey nm)

-- TODO rename unkey and getkey
unKey :: GHC.Name -> Int
unKey = GHC.getKey . GHC.nameUnique

getKey :: Name -> Int
getKey = IntSet.findMin . nameKeys

parseHieFile :: GHC.HieFile -> Either ParseError Module
parseHieFile file@(GHC.HieFile _ mdl _ asts avails _) = do
  Collect decls uses <- collect asts
  tree <- structure decls
  let uses' = Set.fromList $
        flip mapMaybe uses $ \(loc, callee) -> do
          (_, caller) <- ST.lookupInner loc tree
          pure (getKey caller, unKey callee)
      modString = GHC.moduleNameString $ GHC.moduleName mdl
      exportNames = avails >>= GHC.availNames
  pure $ Module modString (IntSet.fromList $ unKey <$> exportNames) (deduplicate tree) uses'

newtype SemanticTree = SemanticTree (Map String (IntSet, DeclType, SemanticTree))

instance Semigroup SemanticTree where
  SemanticTree ta <> SemanticTree tb = SemanticTree $ Map.unionWith f ta tb
    where
      f (ks, typ, sub) (ks', _, sub') = (ks <> ks', typ, sub <> sub')

instance Monoid SemanticTree where mempty = SemanticTree mempty

deduplicate :: STree GHC.RealSrcLoc (DeclType, Name) -> SemanticTree
deduplicate = ST.foldSTree mempty $ \l _ (typ, Name str ks) sub _ r ->
  let this = SemanticTree $ Map.singleton str (ks, typ, sub)
   in l <> this <> r

structure :: [Decl] -> Either ParseError (STree GHC.RealSrcLoc (DeclType, Name))
structure = lexTree
  where
    lexTree :: [Decl] -> Either ParseError (STree GHC.RealSrcLoc (DeclType, Name))
    lexTree =
      foldM
        (\t (Decl ty sp na) -> first TreeError $ ST.insertWith f (GHC.realSrcSpanStart sp) (ty, na) (GHC.realSrcSpanEnd sp) t)
        ST.emptySTree
    f (ta, Name na ka) (tb, Name nb kb)
      | ta == tb && na == nb = Just (ta, Name na (ka <> kb))
      | otherwise = Nothing

collect :: GHC.HieASTs a -> Either ParseError Collect
collect (GHC.HieASTs asts) = execStateT (mapM_ collect' asts) (Collect mempty mempty)
  where
    tellDecl :: Decl -> StateT Collect (Either ParseError) ()
    tellDecl decl = modify $ \(Collect decls uses) -> Collect (decl : decls) uses

    tellUse :: GHC.RealSrcLoc -> GHC.Name -> StateT Collect (Either ParseError) ()
    tellUse loc name = modify $ \(Collect decls uses) -> Collect decls ((loc, name) : uses)

    collect' :: GHC.HieAST a -> StateT Collect (Either ParseError) ()
    collect' (GHC.Node (GHC.NodeInfo anns _ ids) nodeSpan children) =
      if Set.member ("ClsInstD", "InstDecl") anns
        then pure ()
        else do
          forM_ (M.toList ids) $ \case
            (Right name, GHC.IdentifierDetails _ info) ->
              classifyIdentifier
                info
                (\ty sp -> tellDecl $ Decl ty sp (mkName name))
                (tellUse (GHC.realSrcSpanStart nodeSpan) name)
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

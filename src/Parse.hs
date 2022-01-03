{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parse where

import Control.Monad.State
import Data.Bifunctor (first)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as M
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

data FoldNode
  = FNEmpty
  | FNUse Use
  | FNValue Value
  | FNVals Int (NonEmpty Value)
  | FNRec Field
  | FNRecs [Field]
  | FNCon Con
  | FNData Data
  | FNImport String
  | FNClass Class
  deriving (Show)

data FoldError
  = IdentifierError GHC.Span IdentifierError
  | AppendError GHC.Span AppendError
  | StructuralError

foldFile :: GHC.HieFile -> Either FoldError [FoldNode]
foldFile (GHC.HieFile _path _module _types (GHC.HieASTs asts) _info _src) =
  case toList asts of
    [GHC.Node _ _ asts'] -> traverse foldAst asts'
    _ -> Left StructuralError

foldAst :: GHC.HieAST a -> Either FoldError FoldNode
foldAst (GHC.Node (GHC.NodeInfo anns _ ids) span children)
  | Set.member ("HsDerivingClause", "HsDerivingClause") anns = pure FNEmpty
foldAst (GHC.Node (GHC.NodeInfo _ _ ids) span children) = do
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

  first (AppendError span) $ do
    c <- foldM appendNodes FNEmpty cs
    n <- foldM appendNodes FNEmpty ns
    appendNodes n (markScope c)

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
  [GHC.ValBind GHC.RegularBind GHC.ModuleScope _, GHC.RecField GHC.RecFieldDecl _] ->
    pure $ FNRec $ Field name mempty
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
    valuelike = pure $ FNValue $ Value name mempty mempty
    uselike = pure $ FNUse $ Use $ Set.singleton key
    ignore = pure FNEmpty

fromModuleName :: Set GHC.ContextInfo -> Maybe (String -> FoldNode)
fromModuleName ctx = case Set.toAscList ctx of
  [GHC.IEThing GHC.Import] -> pure FNImport
  [GHC.IEThing GHC.ImportAs] -> pure $ const FNEmpty
  _ -> Nothing

data AppendError
  = CombineError FoldNode FoldNode

appendNodes :: FoldNode -> FoldNode -> Either AppendError FoldNode
appendNodes FNEmpty rhs = pure rhs
appendNodes (FNUse a) (FNUse b) = pure $ FNUse (a <> b)
appendNodes (FNCon (Con name uses fields)) (FNUse b) = pure $ FNCon $ Con name (uses <> b) fields
appendNodes (FNCon (Con name uses fields)) (FNRecs recs) = pure $ FNCon $ Con name uses (fields <> recs)
appendNodes (FNCon (Con name uses fields)) (FNRec rec) = pure $ FNCon $ Con name uses (fields <> [rec])
appendNodes (FNData (Data name cons use)) (FNCon con) = pure $ FNData $ Data name (con : cons) use
appendNodes (FNData (Data name cons use)) (FNUse use') = pure $ FNData $ Data name cons (use <> use')
appendNodes (FNRec (Field name uses)) (FNUse use) = pure $ FNRec $ Field name (uses <> use)
appendNodes (FNRec a) (FNRec b) = pure $ FNRecs [b, a]
appendNodes lhs FNEmpty = pure lhs
appendNodes lhs rhs = Left $ CombineError lhs rhs

data Value = Value
  { valName :: Name,
    valChildren :: [Value],
    valUses :: Use
  }
  deriving (Show)

data Class = Class
  { clName :: Name,
    clValues :: [Value]
  }
  deriving (Show)

data Data = Data
  { dtName :: Name,
    dtCons :: [Con],
    dtUses :: Use
  }
  deriving (Show)

data Con = Con
  { conName :: Name,
    conUses :: Use,
    conFields :: [Field]
  }
  deriving (Show)

data Field = Field
  { recName :: Name,
    recUses :: Use
  }
  deriving (Show)

data Name = Name Key String

instance Show Name where
  show (Name _ name) = name

newtype Use = Use (Set Key)
  deriving (Semigroup, Monoid)

instance Show Use where show _ = "<uses>"

names :: GHC.HieAST a -> [GHC.Name]
names = (>>= toList) . M.keys . GHC.nodeIdentifiers . GHC.nodeInfo

unname :: GHC.Name -> Name
unname n = Name (nameKey n) (GHC.occNameString $ GHC.nameOccName n)

nameKey :: GHC.Name -> Key
nameKey = Key . GHC.getKey . GHC.nameUnique

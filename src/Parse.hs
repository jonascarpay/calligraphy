{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parse where

import Control.Monad.State
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

data FoldNode
  = FNEmpty
  | FNUse Use
  | FNVals Int (NonEmpty Value)
  | FNRecs (NonEmpty RecordField)
  | FNCons (NonEmpty Con)
  | FNData [Con] Name
  | FNImport String
  deriving (Show)

-- data FoldError
--   = UnhandledName GHC.Identifier (Set GHC.ContextInfo)

foldFile :: GHC.HieFile -> Either String FoldNode
foldFile (GHC.HieFile _path _module _types (GHC.HieASTs asts) _info _src) =
  case toList asts of
    [GHC.Node _ _ asts'] -> traverse foldAst asts' >>= foldM appendNodes FNEmpty
    _ -> Left "que"

foldAst :: GHC.HieAST a -> Either String FoldNode
foldAst (GHC.Node (GHC.NodeInfo _ _ ids) _span children) = do
  cs <- traverse foldAst children
  ns <- forM (M.toList ids) $ \(idn, GHC.IdentifierDetails _ info) ->
    fromIdentifier idn info
  foldM appendNodes FNEmpty cs

fromIdentifier :: GHC.Identifier -> Set GHC.ContextInfo -> Either String FoldNode
fromIdentifier idn info = Right FNEmpty

-- fromIdentifier :: GHC.Identifier -> Set GHC.ContextInfo -> Either String FoldNode
-- fromIdentifier idn info = Left $ UnhandledName idn

appendNodes :: FoldNode -> FoldNode -> Either String FoldNode
appendNodes FNEmpty rhs = pure rhs
appendNodes lhs rhs = Left $ "Couldn't merge " <> show lhs <> " with " <> show rhs

data Value = Value
  { valName :: Name,
    valChildren :: [Value],
    valUses :: Use
  }
  deriving (Show)

data Con = Con
  { conName :: Name,
    conUses :: Use,
    conFields :: [RecordField]
  }
  deriving (Show)

data RecordField = RecordField
  { recName :: Name,
    recUses :: Use
  }
  deriving (Show)

data Name = Name Key String

instance Show Name where
  show (Name _ name) = name

newtype Use = Use (Set Key)

instance Show Use where show _ = "<uses>"

names :: GHC.HieAST a -> [GHC.Name]
names = (>>= toList) . M.keys . GHC.nodeIdentifiers . GHC.nodeInfo

unname :: GHC.Name -> (Key, String)
unname n = (nameKey n, GHC.occNameString $ GHC.nameOccName n)

nameKey :: GHC.Name -> Key
nameKey = Key . GHC.getKey . GHC.nameUnique

data DataType = DataType
  { dtKey :: Key,
    dtName :: String,
    dtCons :: [DataCon]
  }

data DataCon = DataCon
  { dcKey :: Key,
    dcName :: String,
    dcBody :: DataConBody
  }

data DataConBody
  = DataConRecord [(Key, String, [Key])]
  | DataConNaked [Key]

module Filter where

import Control.Monad.State
import Data.EnumMap (EnumMap)
import Data.EnumMap qualified as EnumMap
import Data.Tree
import Parse
import Prelude hiding (filter)

newtype Rep = Rep {unRep :: Int}

filter :: FilterConfig -> Modules -> Modules
filter = undefined

data CollapseConfig = CollapseConfig
  { collapseValues :: Bool,
    collapseClasses :: Bool,
    collapseConstructors :: Bool,
    collapseData :: Bool
  }

data FilterConfig = FilterConfig
  { collapseConfig :: CollapseConfig,
    onlyExports :: Bool,
    depRoot :: Maybe [String],
    revDepRoot :: Maybe [String]
  }

collapse :: CollapseConfig -> Modules -> Modules
collapse cfg (Modules forests calls) =
  let (forests', reps) = flip runState mempty $ (traverse . traverse . traverse) go forests
   in Modules forests' (mapMaybeSet calls $ \(caller, callee) -> _)
  where
    shouldCollapse :: DeclType -> CollapseConfig -> Bool
    shouldCollapse ValueDecl (CollapseConfig True _ _ _) = True
    shouldCollapse ConDecl (CollapseConfig _ True _ _) = True
    shouldCollapse DataDecl (CollapseConfig _ _ True _) = True
    shouldCollapse ClassDecl (CollapseConfig _ _ _ True) = True
    shouldCollapse _ _ = False

    assoc :: Key -> Rep -> State (EnumMap Key Rep) ()
    assoc key rep = modify (EnumMap.insert key rep)

    collapseNode :: Key -> Tree Decl -> State (EnumMap Key Key) ()
    collapseNode rep = go
      where
        go :: Tree Decl -> State (EnumMap Key Key) ()
        go (Node decl children) = modify (EnumMap.insert (declKey decl) rep) >> mapM_ go children

    go :: Tree Decl -> State (EnumMap Key Key) (Tree Decl)
    go (Node decl children)
      | shouldCollapse (declType decl) cfg = Node decl [] <$ forM_ children (collapseNode (declKey decl))
      | otherwise = Node decl <$> mapM go children

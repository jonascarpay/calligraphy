module Calligraphy.Compat.Debug
  ( ppHieFile,
    ppIdentifier,
    showGHCName,
  )
where

import qualified Calligraphy.Compat.GHC as GHC
import Calligraphy.Compat.Lib
import Calligraphy.Util.Printer
import Control.Monad
import qualified Data.Map as Map

-- TODO Better specialize to different HIE versions, that's the point of splitting this
ppHieFile :: Prints GHC.HieFile
ppHieFile (GHC.HieFile _ mdl _types (GHC.HieASTs asts) _exps _src) = do
  strLn $ showModuleName $ GHC.moduleName mdl
  indent $ forM_ asts ppNameTree
  where
    ppNameTree :: GHC.HieAST a -> Printer ()
    ppNameTree node@(GHC.Node _ spn children) =
      forNodeInfos_ node $ \nodeInfo -> do
        strLn $ ">> " <> showSpan spn <> " " <> showAnns nodeInfo
        indent $ do
          let pids = fmap GHC.identInfo <$> Map.toList (GHC.nodeIdentifiers nodeInfo)
          forM_ pids $ \(idn, ctxInfo) -> do
            ppIdentifier idn
            indent $ mapM_ (strLn . showContextInfo) ctxInfo
          forM_ children ppNameTree

showModuleName :: GHC.ModuleName -> String
showModuleName = flip mappend " (module)" . show . GHC.moduleNameString

showSpan :: GHC.RealSrcSpan -> String
showSpan s =
  mconcat
    [ show $ GHC.srcSpanStartLine s,
      ":",
      show $ GHC.srcSpanStartCol s,
      " - ",
      show $ GHC.srcSpanEndLine s,
      ":",
      show $ GHC.srcSpanEndCol s
    ]

ppIdentifier :: Prints GHC.Identifier
ppIdentifier = strLn . either showModuleName showGHCName

showGHCName :: GHC.Name -> String
showGHCName name = GHC.getOccString name <> "    " <> show (GHC.getKey $ GHC.nameUnique name)

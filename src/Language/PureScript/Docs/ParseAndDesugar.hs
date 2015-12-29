{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.PureScript.Docs.ParseAndDesugar
  ( parseAndDesugar
  ) where

import Prelude ()
import Prelude.Compat

import qualified Data.Map as M
import Control.Arrow (first)
import Control.Monad

import Control.Monad.Writer.Strict (runWriterT)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))

import Web.Bower.PackageMeta (PackageName)

import qualified Language.PureScript as P
import qualified Language.PureScript.Constants as C
import Language.PureScript.Docs.Types
import Language.PureScript.Docs.Convert (collectBookmarks)

-- |
-- Given:
--
--    * A list of local source files
--    * A list of source files from external dependencies, together with their
--      package names
--
-- This function does the following:
--
--    * Parse all of the input and dependency source files
--    * Associate each dependency module with its package name, thereby
--      distinguishing these from local modules
--    * Partially desugar all of the resulting modules (just enough for
--      producing documentation from them)
--    * Collect a list of bookmarks from the whole set of source files
--    * Return the desugared modules, the bookmarks, and the imports/exports
--      Env (which is needed for producing documentation).
parseAndDesugar ::
  (Functor m, Applicative m, MonadError P.MultipleErrors m, MonadIO m) =>
  [FilePath]
  -> [(PackageName, FilePath)]
  -> m ([InPackage P.Module], [Bookmark], P.Env)
parseAndDesugar inputFiles depsFiles = do
  inputFiles' <- traverse (parseAs Local) inputFiles
  depsFiles'  <- traverse (\(pkgName, f) -> parseAs (FromDep pkgName) f) depsFiles

  ms  <- parseFiles (inputFiles' ++ depsFiles')
  ms' <- sortModules (map snd ms)
  desugarWithBookmarks ms ms'

parseFiles ::
  (MonadError P.MultipleErrors m, MonadIO m) =>
  [(FileInfo, FilePath)]
  -> m [(FileInfo, P.Module)]
parseFiles =
  throwLeft . P.parseModulesFromFiles fileInfoToString

sortModules ::
  (Functor m, MonadError P.MultipleErrors m, MonadIO m) =>
  [P.Module]
  -> m [P.Module]
sortModules =
  fmap fst . throwLeft . sortModules' . map importPrim
  where
  sortModules' :: [P.Module] -> Either P.MultipleErrors ([P.Module], P.ModuleGraph)
  sortModules' = P.sortModules

desugarWithBookmarks ::
  (MonadError P.MultipleErrors m, MonadIO m) =>
  [(FileInfo, P.Module)]
  -> [P.Module]
  -> m ([InPackage P.Module], [Bookmark], P.Env)
desugarWithBookmarks msInfo msSorted =  do
  (env, msDesugared) <- throwLeft (desugar msSorted)

  let msDeps = getDepsModuleNames (map (\(fp, m) -> (,m) <$> fp) msInfo)
      msPackages = map (addPackage msDeps) msDesugared
      bookmarks = concatMap collectBookmarks msPackages

  return (msPackages, bookmarks, env)

throwLeft :: (MonadError l m) => Either l r -> m r
throwLeft = either throwError return

-- | Specifies whether a PureScript source file is considered as:
--
-- 1) with the `Local` constructor, a target source file, i.e., we want to see
--    its modules in the output
-- 2) with the `FromDep` constructor, a dependencies source file, i.e. we do
--    not want its modules in the output; it is there to enable desugaring, and
--    to ensure that links between modules are constructed correctly.
type FileInfo = InPackage FilePath

fileInfoToString :: FileInfo -> FilePath
fileInfoToString (Local fn) = fn
fileInfoToString (FromDep _ fn) = fn

importPrim :: P.Module -> P.Module
importPrim = P.addDefaultImport (P.ModuleName [P.ProperName C.prim])

desugar ::
  (Functor m, Applicative m, MonadError P.MultipleErrors m) =>
  [P.Module]
  -> m (P.Env, [P.Module])
desugar = P.evalSupplyT 0 . desugar'
  where
  desugar' =
    traverse P.desugarDoModule
      >=> P.desugarCasesModule
      >=> ignoreWarnings . P.desugarImportsWithEnv []

  ignoreWarnings m = liftM fst (runWriterT m)

parseFile :: FilePath -> IO (FilePath, String)
parseFile input' = (,) input' <$> readFile input'

parseAs :: (Functor m, MonadIO m) => (FilePath -> a) -> FilePath -> m (a, String)
parseAs g = fmap (first g) . liftIO . parseFile

getDepsModuleNames :: [InPackage (FilePath, P.Module)] -> M.Map P.ModuleName PackageName
getDepsModuleNames = foldl go M.empty
  where
  go deps p = deps # case p of
    Local _ -> id
    FromDep pkgName (_, m) -> M.insert (P.getModuleName m) pkgName
  (#) = flip ($)

addPackage :: M.Map P.ModuleName PackageName -> P.Module -> InPackage P.Module
addPackage depsModules m =
  case M.lookup (P.getModuleName m) depsModules of
    Just pkgName -> FromDep pkgName m
    Nothing -> Local m

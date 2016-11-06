module Language.PureScript.Docs.ParseAndBookmark
  ( parseAndBookmark
  ) where

import Prelude.Compat

import Control.Arrow (first)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Map as M
import Data.Text (Text)

import Language.PureScript.Docs.Convert (collectBookmarks)
import Language.PureScript.Docs.Types
import qualified Language.PureScript as P
import System.IO.UTF8 (readUTF8FileT)
import Web.Bower.PackageMeta (PackageName)

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
--    * Collect a list of bookmarks from the whole set of source files
--    * Return the parsed modules and the bookmarks
parseAndBookmark ::
  (MonadError P.MultipleErrors m, MonadIO m) =>
  [FilePath]
  -> [(PackageName, FilePath)]
  -> m ([InPackage P.Module], [Bookmark])
parseAndBookmark inputFiles depsFiles = do
  inputFiles' <- traverse (parseAs Local) inputFiles
  depsFiles'  <- traverse (\(pkgName, f) -> parseAs (FromDep pkgName) f) depsFiles

  addBookmarks <$> parseFiles (inputFiles' ++ depsFiles')

parseFiles ::
  (MonadError P.MultipleErrors m) =>
  [(FileInfo, Text)]
  -> m [(FileInfo, P.Module)]
parseFiles =
  throwLeft . P.parseModulesFromFiles fileInfoToString

addBookmarks ::
  [(FileInfo, P.Module)]
  -> ([InPackage P.Module], [Bookmark])
addBookmarks msInfo =
  let
    msDeps = getDepsModuleNames (map (\(fp, m) -> (,m) <$> fp) msInfo)
    msPackages = map (addPackage msDeps . snd) msInfo
    bookmarks = concatMap collectBookmarks msPackages
  in
    (msPackages, bookmarks)

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

parseFile :: FilePath -> IO (FilePath, Text)
parseFile input' = (,) input' <$> readUTF8FileT input'

parseAs :: (MonadIO m) => (FilePath -> a) -> FilePath -> m (a, Text)
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

module Language.PureScript.Docs.ParseInPackage
  ( parseFilesInPackages
  ) where

import Protolude

import qualified Data.Map as M

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
--    * Return the parsed modules and a Map mapping module names to package
--      names for modules which come from dependencies. If a module does not
--      exist in the map, it can safely be assumed to be local.
parseFilesInPackages ::
  (MonadError P.MultipleErrors m, MonadIO m) =>
  [FilePath]
  -> [(PackageName, FilePath)]
  -> m ([P.Module], Map P.ModuleName PackageName)
parseFilesInPackages inputFiles depsFiles = do
  inputFiles' <- traverse (readFileAs . Local) inputFiles
  depsFiles'  <- traverse (readFileAs . uncurry FromDep) depsFiles

  modules <- parse (inputFiles' ++ depsFiles')

  let mnMap = M.fromList (mapMaybe (\(inpkg, m) -> (P.getModuleName m,) <$> inPkgToMaybe inpkg) modules)

  pure (map snd modules, mnMap)

  where
  parse ::
    (MonadError P.MultipleErrors m) =>
    [(FileInfo, Text)]
    -> m [(FileInfo, P.Module)]
  parse =
    throwLeft . P.parseModulesFromFiles fileInfoToString

  inPkgToMaybe = \case
    Local _ -> Nothing
    FromDep pkgName _ -> Just pkgName

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

readFileAs :: (MonadIO m) => FileInfo -> m (FileInfo, Text)
readFileAs fi = liftIO . fmap ((fi,)) $ readUTF8FileT (ignorePackage fi)


module Language.PureScript.Docs.Collect
  ( collectDocs
  ) where

import Protolude hiding (check)

import Control.Arrow ((&&&))
import qualified Data.Aeson.BetterErrors as ABE
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.String (String)
import qualified Data.Set as Set
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.UTF8 (readUTF8FileT, readUTF8FilesT)

import Language.PureScript.Docs.Convert.ReExports (updateReExports)
import Language.PureScript.Docs.Prim (primModules)
import Language.PureScript.Docs.Types

import qualified Language.PureScript.AST as P
import qualified Language.PureScript.CST as P
import qualified Language.PureScript.Crash as P
import qualified Language.PureScript.Errors as P
import qualified Language.PureScript.Externs as P
import qualified Language.PureScript.Make as P
import qualified Language.PureScript.Names as P
import qualified Language.PureScript.Options as P

import Web.Bower.PackageMeta (PackageName)

-- |
-- Given a compiler output directory, a list of input PureScript source files,
-- and a list of dependency PureScript source files, produce documentation for
-- the input files in the intermediate documentation format. Note that
-- dependency files are not included in the result.
--
-- If the output directory is not up to date with respect to the provided input
-- and dependency files, the files will be built as if with just the "docs"
-- codegen target, i.e. "purs compile --codegen docs".
--
collectDocs ::
  forall m.
  (MonadError P.MultipleErrors m, MonadIO m) =>
  FilePath ->
  [FilePath] ->
  [(PackageName, FilePath)] ->
  m ([(FilePath, Module)], Map P.ModuleName PackageName)
collectDocs outputDir inputFiles depsFiles = do
  (modulePaths, modulesDeps) <- getModulePackageInfo inputFiles depsFiles
  externs <- compileForDocs outputDir (map fst modulePaths)

  let (withPackage, shouldKeep) =
        packageDiscriminators modulesDeps
  let go =
        operateAndRetag identity modName $ \mns -> do
          docsModules <- traverse (liftIO . parseDocsJsonFile outputDir) mns
          addReExports withPackage docsModules externs

  docsModules <- go modulePaths
  pure ((filter (shouldKeep . modName . snd) docsModules), modulesDeps)

  where
  packageDiscriminators modulesDeps =
    let
      shouldKeep mn = isLocal mn && not (P.isBuiltinModuleName mn)

      withPackage :: P.ModuleName -> InPackage P.ModuleName
      withPackage mn =
        case Map.lookup mn modulesDeps of
          Just pkgName -> FromDep pkgName mn
          Nothing -> Local mn

      isLocal :: P.ModuleName -> Bool
      isLocal = not . flip Map.member modulesDeps
    in
      (withPackage, shouldKeep)

-- |
-- Compile with just the 'docs' codegen target, writing results into the given
-- output directory.
--
compileForDocs ::
  forall m.
  (MonadError P.MultipleErrors m, MonadIO m) =>
  FilePath ->
  [FilePath] ->
  m [P.ExternsFile]
compileForDocs outputDir inputFiles = do
  result <- liftIO $ do
    moduleFiles <- readUTF8FilesT inputFiles
    fmap fst $ P.runMake testOptions $ do
      ms <- P.parseModulesFromFiles identity moduleFiles
      let filePathMap = Map.fromList $ map (\(fp, pm) -> (P.getModuleName $ P.resPartial pm, Right fp)) ms
      foreigns <- P.inferForeignModules filePathMap
      let makeActions =
            (P.buildMakeActions outputDir filePathMap foreigns False)
              { P.progress = liftIO . putStrLn . renderProgressMessage
              }
      P.make makeActions (map snd ms)
  either throwError return result

  where
  renderProgressMessage :: P.ProgressMessage -> String
  renderProgressMessage (P.CompilingModule mn) =
    "Compiling documentation for " ++ T.unpack (P.runModuleName mn)

  testOptions :: P.Options
  testOptions = P.defaultOptions { P.optionsCodegenTargets = Set.singleton P.Docs }

parseDocsJsonFile :: FilePath -> P.ModuleName -> IO Module
parseDocsJsonFile outputDir mn =
  let
    filePath = outputDir </> T.unpack (P.runModuleName mn) </> "docs.json"
  in do
    str <- BS.readFile filePath
    case ABE.parseStrict asModule str of
      Right m -> pure m
      Left err -> P.internalError $
        "Failed to decode: " ++ filePath ++
        intercalate "\n" (map T.unpack (ABE.displayError displayPackageError err))

addReExports ::
  (MonadError P.MultipleErrors m) =>
  (P.ModuleName -> InPackage P.ModuleName) ->
  [Module] ->
  [P.ExternsFile] ->
  m [Module]
addReExports withPackage docsModules externs = do
  -- We add the Prim docs modules here, so that docs generation is still
  -- possible if the modules we are generating docs for re-export things from
  -- Prim submodules. Note that the Prim modules do not exist as
  -- @Language.PureScript.Module@ values because they do not contain anything
  -- that exists at runtime. However, we have pre-constructed
  -- @Language.PureScript.Docs.Types.Module@ values for them, which we use
  -- here.
  let moduleMap =
        Map.fromList
          (map (modName &&& identity)
               (docsModules ++ primModules))

  let withReExports = updateReExports externs withPackage moduleMap
  pure (Map.elems withReExports)

-- |
-- Perform an operation on a list of things which are tagged, and reassociate
-- the things with their tags afterwards.
--
operateAndRetag ::
  forall m a b key tag.
  Monad m =>
  Ord key =>
  Show key =>
  (a -> key) ->
  (b -> key) ->
  ([a] -> m [b]) ->
  [(tag, a)] ->
  m [(tag, b)]
operateAndRetag keyA keyB operation input =
  fmap (map retag) $ operation (map snd input)
  where
  tags :: Map key tag
  tags = Map.fromList $ map (\(tag, a) -> (keyA a, tag)) input

  findTag :: key -> tag
  findTag key =
    case Map.lookup key tags of
      Just tag -> tag
      Nothing -> P.internalError ("Missing tag for: " ++ show key)

  retag :: b -> (tag, b)
  retag b = (findTag (keyB b), b)

-- |
-- Given:
--
--    * A list of local source files
--    * A list of source files from external dependencies, together with their
--      package names
--
-- This function does the following:
--
--    * Partially parse all of the input and dependency source files to get
--      the module name of each module
--    * Associate each dependency module with its package name, thereby
--      distinguishing these from local modules
--    * Return the file paths paired with the names of the modules they
--      contain, and a Map of module names to package names for modules which
--      come from dependencies. If a module does not exist in the map, it can
--      safely be
--      assumed to be local.
getModulePackageInfo ::
  (MonadError P.MultipleErrors m, MonadIO m) =>
  [FilePath]
  -> [(PackageName, FilePath)]
  -> m ([(FilePath, P.ModuleName)], Map P.ModuleName PackageName)
getModulePackageInfo inputFiles depsFiles = do
  inputFiles' <- traverse (readFileAs . Local) inputFiles
  depsFiles'  <- traverse (readFileAs . uncurry FromDep) depsFiles

  moduleNames <- getModuleNames (inputFiles' ++ depsFiles')

  let mnMap =
        Map.fromList $
          mapMaybe (\(pkgPath, mn) -> (mn,) <$> getPkgName pkgPath) moduleNames

  pure (map (first ignorePackage) moduleNames, mnMap)

  where
  getModuleNames ::
    (MonadError P.MultipleErrors m) =>
    [(InPackage FilePath, Text)]
    -> m [(InPackage FilePath, P.ModuleName)]
  getModuleNames =
    fmap (map (second (P.getModuleName . P.resPartial)))
    . either throwError return
    . P.parseModulesFromFiles ignorePackage

  getPkgName = \case
    Local _ -> Nothing
    FromDep pkgName _ -> Just pkgName

  readFileAs ::
    (MonadIO m) =>
    InPackage FilePath ->
    m (InPackage FilePath, Text)
  readFileAs fi =
    liftIO . fmap ((fi,)) $ readUTF8FileT (ignorePackage fi)

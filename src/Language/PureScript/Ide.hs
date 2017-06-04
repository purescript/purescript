-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide
-- Description : Interface for the psc-ide-server
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Interface for the psc-ide-server
-----------------------------------------------------------------------------

{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE TemplateHaskell       #-}

module Language.PureScript.Ide
       ( handleCommand
       ) where

import           Protolude

import           "monad-logger" Control.Monad.Logger
import qualified Language.PureScript                as P
import qualified Language.PureScript.Ide.CaseSplit  as CS
import           Language.PureScript.Ide.Command
import           Language.PureScript.Ide.Completion
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Externs
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Imports    hiding (Import)
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Pursuit
import           Language.PureScript.Ide.Rebuild
import           Language.PureScript.Ide.SourceFile
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import           System.Directory (getCurrentDirectory, getDirectoryContents, doesDirectoryExist, doesFileExist)
import           System.FilePath ((</>))
import           System.FilePath.Glob (glob)

-- | Accepts a Commmand and runs it against psc-ide's State. This is the main
-- entry point for the server.
handleCommand :: (Ide m, MonadLogger m, MonadError IdeError m) =>
                 Command -> m Success
handleCommand c = case c of
  Load [] ->
    findAvailableExterns >>= loadModulesAsync
  Load modules ->
    loadModulesAsync modules
  LoadSync [] ->
    findAvailableExterns >>= loadModulesSync
  LoadSync modules ->
    loadModulesSync modules
  Type search filters currentModule ->
    findType search filters currentModule
  Complete filters matcher currentModule complOptions ->
    findCompletions filters matcher currentModule complOptions
  Pursuit query Package ->
    findPursuitPackages query
  Pursuit query Identifier ->
    findPursuitCompletions query
  List LoadedModules ->
    printModules
  List AvailableModules ->
    listAvailableModules
  List (Imports fp) ->
    ImportList <$> parseImportsFromFile fp
  CaseSplit l b e wca t ->
    caseSplit l b e wca t
  AddClause l wca ->
    MultilineTextResult <$> CS.addClause l wca
  Import fp outfp _ (AddImplicitImport mn) -> do
    rs <- addImplicitImport fp mn
    answerRequest outfp rs
  Import fp outfp _ (AddQualifiedImport mn qual) -> do
    rs <- addQualifiedImport fp mn qual
    answerRequest outfp rs
  Import fp outfp filters (AddImportForIdentifier ident) -> do
    rs <- addImportForIdentifier fp ident filters
    case rs of
      Right rs' -> answerRequest outfp rs'
      Left question ->
        pure (CompletionResult (map (completionFromMatch . simpleExport . map withEmptyAnn) question))
  Rebuild file ->
    rebuildFileAsync file
  RebuildSync file ->
    rebuildFileSync file
  Cwd ->
    TextResult . toS <$> liftIO getCurrentDirectory
  Reset ->
    resetIdeState $> TextResult "State has been reset."
  Quit ->
    liftIO exitSuccess

findCompletions
  :: Ide m
  => [Filter]
  -> Matcher IdeDeclarationAnn
  -> Maybe P.ModuleName
  -> CompletionOptions
  -> m Success
findCompletions filters matcher currentModule complOptions = do
  modules <- getAllModules currentModule
  pure (CompletionResult (getCompletions filters matcher complOptions modules))

findType :: Ide m =>
            Text -> [Filter] -> Maybe P.ModuleName -> m Success
findType search filters currentModule = do
  modules <- getAllModules currentModule
  pure (CompletionResult (getExactCompletions search filters modules))

findPursuitCompletions :: MonadIO m =>
                          PursuitQuery -> m Success
findPursuitCompletions (PursuitQuery q) =
  PursuitResult <$> liftIO (searchPursuitForDeclarations q)

findPursuitPackages :: MonadIO m =>
                       PursuitQuery -> m Success
findPursuitPackages (PursuitQuery q) =
  PursuitResult <$> liftIO (findPackagesForModuleIdent q)

printModules :: Ide m => m Success
printModules = ModuleList . map P.runModuleName <$> getLoadedModulenames

outputDirectory :: Ide m => m FilePath
outputDirectory = do
  outputPath <- confOutputPath . ideConfiguration <$> ask
  cwd <- liftIO getCurrentDirectory
  pure (cwd </> outputPath)

listAvailableModules :: Ide m => m Success
listAvailableModules = do
  oDir <- outputDirectory
  liftIO $ do
    contents <- getDirectoryContents oDir
    let cleaned = filter (`notElem` [".", ".."]) contents
    return (ModuleList (map toS cleaned))

caseSplit :: (Ide m, MonadError IdeError m) =>
  Text -> Int -> Int -> CS.WildcardAnnotations -> Text -> m Success
caseSplit l b e csa t = do
  patterns <- CS.makePattern l b e csa <$> CS.caseSplit t
  pure (MultilineTextResult patterns)

-- | Finds all the externs.json files inside the output folder and returns the
-- corresponding Modulenames
findAvailableExterns :: (Ide m, MonadError IdeError m) => m [P.ModuleName]
findAvailableExterns = do
  oDir <- outputDirectory
  unlessM (liftIO (doesDirectoryExist oDir))
    (throwError (GeneralError "Couldn't locate your output directory."))
  liftIO $ do
    directories <- getDirectoryContents oDir
    moduleNames <- filterM (containsExterns oDir) directories
    pure (P.moduleNameFromString . toS <$> moduleNames)
  where
    -- Takes the output directory and a filepath like "Monad.Control.Eff" and
    -- looks up, whether that folder contains an externs.json
    containsExterns :: FilePath -> FilePath -> IO Bool
    containsExterns oDir d
      | d `elem` [".", ".."] = pure False
      | otherwise = do
          let file = oDir </> d </> "externs.json"
          doesFileExist file

-- | Finds all matches for the globs specified at the commandline
findAllSourceFiles :: Ide m => m [FilePath]
findAllSourceFiles = do
  globs <- confGlobs . ideConfiguration <$> ask
  liftIO (concatMapM glob globs)

-- | Looks up the ExternsFiles for the given Modulenames and loads them into the
-- server state. Then proceeds to parse all the specified sourcefiles and
-- inserts their ASTs into the state. Finally kicks off an async worker, which
-- populates the VolatileState.
loadModulesAsync
  :: (Ide m, MonadError IdeError m, MonadLogger m)
  => [P.ModuleName]
  -> m Success
loadModulesAsync moduleNames = do
  tr <- loadModules moduleNames

  -- Finally we kick off the worker with @async@ and return the number of
  -- successfully parsed modules.
  env <- ask
  let ll = confLogLevel (ideConfiguration env)
  -- populateVolatileState return Unit for now, so it's fine to discard this
  -- result. We might want to block on this in a benchmarking situation.
  _ <- liftIO (async (runLogger ll (runReaderT populateVolatileState env)))
  pure tr

loadModulesSync
  :: (Ide m, MonadError IdeError m, MonadLogger m)
  => [P.ModuleName]
  -> m Success
loadModulesSync moduleNames = do
  tr <- loadModules moduleNames
  populateVolatileState
  pure tr

loadModules
  :: (Ide m, MonadError IdeError m, MonadLogger m)
  => [P.ModuleName]
  -> m Success
loadModules moduleNames = do
  -- We resolve all the modulenames to externs files and load these into memory.
  oDir <- outputDirectory
  let efPaths =
        map (\mn -> oDir </> toS (P.runModuleName mn) </> "externs.json") moduleNames
  efiles <- traverse readExternFile efPaths
  traverse_ insertExterns efiles

  -- We parse all source files, log eventual parse failures and insert the
  -- successful parses into the state.
  (failures, allModules) <-
    partitionEithers <$> (parseModulesFromFiles =<< findAllSourceFiles)
  unless (null failures) $
    $(logWarn) ("Failed to parse: " <> show failures)
  traverse_ insertModule allModules

  pure (TextResult ("Loaded " <> show (length efiles) <> " modules and "
                    <> show (length allModules) <> " source files."))

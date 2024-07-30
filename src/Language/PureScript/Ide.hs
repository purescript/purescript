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

{-# LANGUAGE PackageImports #-}

module Language.PureScript.Ide
       ( handleCommand
       ) where

import Protolude hiding (moduleName)

import "monad-logger" Control.Monad.Logger (MonadLogger, logWarnN)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.PureScript qualified as P
import Language.PureScript.Glob (toInputGlobs, PSCGlobs(..))
import Language.PureScript.Ide.CaseSplit qualified as CS
import Language.PureScript.Ide.Command (Command(..), ImportCommand(..), ListType(..))
import Language.PureScript.Ide.Completion (CompletionOptions, completionFromMatch, getCompletions, getExactCompletions, simpleExport)
import Language.PureScript.Ide.Error (IdeError(..))
import Language.PureScript.Ide.Externs (readExternFile)
import Language.PureScript.Ide.Filter (Filter)
import Language.PureScript.Ide.Imports (parseImportsFromFile)
import Language.PureScript.Ide.Imports.Actions (addImplicitImport, addImportForIdentifier, addQualifiedImport, answerRequest)
import Language.PureScript.Ide.Matcher (Matcher)
import Language.PureScript.Ide.Prim (idePrimDeclarations)
import Language.PureScript.Ide.Rebuild (rebuildFileAsync, rebuildFileSync)
import Language.PureScript.Ide.SourceFile (parseModulesFromFiles)
import Language.PureScript.Ide.State (getAllModules, getLoadedModulenames, insertExterns, insertModule, populateVolatileState, populateVolatileStateSync, resetIdeState, setFocusedModules, getFocusedModules)
import Language.PureScript.Ide.Types (Annotation(..), Ide, IdeConfiguration(..), IdeDeclarationAnn(..), IdeEnvironment(..), Success(..))
import Language.PureScript.Ide.Util (discardAnn, identifierFromIdeDeclaration, namespaceForDeclaration, withEmptyAnn)
import Language.PureScript.Ide.Usage (findUsages)
import System.Directory (getCurrentDirectory, getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), normalise)

-- | Accepts a Command and runs it against psc-ide's State. This is the main
-- entry point for the server.
handleCommand
  :: (Ide m, MonadLogger m, MonadError IdeError m)
  => Command
  -> m Success
handleCommand c = case c of
  Load [] ->
    -- Clearing the State before populating it to avoid a space leak
    resetIdeState *> findAvailableExterns >>= loadModulesAsync
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
  List LoadedModules -> do
    logWarnN
      "Listing the loaded modules command is DEPRECATED, use the completion command and filter it to modules instead"
    printModules
  List AvailableModules ->
    listAvailableModules
  List (Imports fp) ->
    ImportList <$> parseImportsFromFile fp
  CaseSplit l b e wca t ->
    caseSplit l b e wca t
  AddClause l wca ->
    MultilineTextResult <$> CS.addClause l wca
  FindUsages moduleName ident namespace -> do
    Map.lookup moduleName <$> getAllModules Nothing >>= \case
      Nothing -> throwError (GeneralError "Module not found")
      Just decls -> do
        case find (\d -> namespaceForDeclaration (discardAnn d) == namespace
                    && identifierFromIdeDeclaration (discardAnn d) == ident) decls of
          Nothing -> throwError (GeneralError "Declaration not found")
          Just declaration -> do
            let sourceModule = fromMaybe moduleName (declaration & _idaAnnotation & _annExportedFrom)
            UsagesResult . foldMap toList <$> findUsages (discardAnn declaration) sourceModule
  Import fp outfp _ (AddImplicitImport mn) -> do
    rs <- addImplicitImport fp mn
    answerRequest outfp rs
  Import fp outfp _ (AddQualifiedImport mn qual) -> do
    rs <- addQualifiedImport fp mn qual
    answerRequest outfp rs
  Import fp outfp filters (AddImportForIdentifier ident qual) -> do
    rs <- addImportForIdentifier fp ident qual filters
    case rs of
      Right rs' -> answerRequest outfp rs'
      Left question ->
        pure (CompletionResult (map (completionFromMatch . simpleExport . map withEmptyAnn) question))
  Rebuild file actualFile targets ->
    rebuildFileAsync file actualFile targets
  RebuildSync file actualFile targets ->
    rebuildFileSync file actualFile targets
  Focus modulesToFocus ->
    setFocusedModules modulesToFocus $> TextResult "Focused modules have been set."
  Cwd ->
    TextResult . T.pack <$> liftIO getCurrentDirectory
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
  let insertPrim = Map.union idePrimDeclarations
  pure (CompletionResult (getCompletions filters matcher complOptions (insertPrim modules)))

findType
  :: Ide m
  => Text
  -> [Filter]
  -> Maybe P.ModuleName
  -> m Success
findType search filters currentModule = do
  modules <- getAllModules currentModule
  let insertPrim = Map.union idePrimDeclarations
  pure (CompletionResult (getExactCompletions search filters (insertPrim modules)))

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

-- | Finds all the externs inside the output folder and returns the
-- corresponding module names
findAvailableExterns :: (Ide m, MonadError IdeError m) => m [P.ModuleName]
findAvailableExterns = do
  oDir <- outputDirectory
  unlessM (liftIO (doesDirectoryExist oDir))
    (throwError (GeneralError $ "Couldn't locate your output directory at: " <> T.pack (normalise oDir)))
  liftIO $ do
    directories <- getDirectoryContents oDir
    moduleNames <- filterM (containsExterns oDir) directories
    pure (P.moduleNameFromString . toS <$> moduleNames)
  where
    -- Takes the output directory and a filepath like "Data.Array" and
    -- looks up, whether that folder contains an externs file
    containsExterns :: FilePath -> FilePath -> IO Bool
    containsExterns oDir d
      | d `elem` [".", ".."] = pure False
      | otherwise = do
          let file = oDir </> d </> P.externsFileName
          doesFileExist file

-- | Finds all matches for the globs specified at the commandline
findAllSourceFiles :: Ide m => m [FilePath]
findAllSourceFiles = do
  IdeConfiguration{..} <- ideConfiguration <$> ask
  liftIO $ toInputGlobs $ PSCGlobs
    { pscInputGlobs = confGlobs
    , pscInputGlobsFromFile = confGlobsFromFile
    , pscExcludeGlobs = confGlobsExclude
    , pscWarnFileTypeNotFound = const $ pure ()
    }


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
  _ <- populateVolatileState
  pure tr

loadModulesSync
  :: (Ide m, MonadError IdeError m, MonadLogger m)
  => [P.ModuleName]
  -> m Success
loadModulesSync moduleNames = do
  tr <- loadModules moduleNames
  populateVolatileStateSync
  pure tr

loadModules
  :: (Ide m, MonadError IdeError m, MonadLogger m)
  => [P.ModuleName]
  -> m Success
loadModules moduleNames = do
  focusedModules <- getFocusedModules
  -- We resolve all the modulenames to externs files and load these into memory.
  oDir <- outputDirectory
  let
    -- But we only load the externs files that are in the focusedModules.
    efModules = 
        if Set.null focusedModules then 
          moduleNames
        else 
          Set.toList $ Set.fromList moduleNames `Set.intersection` focusedModules
    efPaths =
        map (\mn -> oDir </> toS (P.runModuleName mn) </> P.externsFileName) efModules
  efiles <- traverse readExternFile efPaths
  traverse_ insertExterns efiles

  -- We parse all source files, log eventual parse failures and insert the
  -- successful parses into the state.
  (failures, allModules) <-
    partitionEithers <$> (parseModulesFromFiles =<< findAllSourceFiles)
  unless (null failures) $
    logWarnN ("Failed to parse: " <> show failures)
  traverse_ insertModule allModules

  pure (TextResult ("Loaded " <> show (length efiles) <> " modules and "
                    <> show (length allModules) <> " source files."))

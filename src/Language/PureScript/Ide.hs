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
import Data.Text qualified as T
import Language.PureScript qualified as P
import Language.PureScript.Glob (toInputGlobs, PSCGlobs(..))
import Language.PureScript.Ide.CaseSplit qualified as CS
import Language.PureScript.Ide.Command (Command(..), ImportCommand(..), ListType(..))
import Language.PureScript.Ide.Completion (CompletionOptions (coMaxResults), completionFromMatch, getCompletions, getExactCompletions, simpleExport)
import Language.PureScript.Ide.Error (IdeError(..))
import Language.PureScript.Ide.Externs (readExternFile)
import Language.PureScript.Ide.Filter qualified as F
import Language.PureScript.Ide.Imports (parseImportsFromFile)
import Language.PureScript.Ide.Imports.Actions (addImplicitImport, addImportForIdentifier, addQualifiedImport, answerRequest)
import Language.PureScript.Ide.Matcher (Matcher)
import Language.PureScript.Ide.Prim (idePrimDeclarations)
import Language.PureScript.Ide.Rebuild (rebuildFileAsync, rebuildFileSync)
import Language.PureScript.Ide.SourceFile (parseModulesFromFiles)
import Language.PureScript.Ide.State (getAllModules, getLoadedModulenames, insertExterns, insertModule, populateVolatileState, populateVolatileStateSync, resetIdeState, getSqliteFilePath, runQuery)
import Language.PureScript.Ide.Types (Annotation(..), Ide, IdeConfiguration(..), IdeDeclarationAnn(..), IdeEnvironment(..), Success(..), Completion (..))
import Language.PureScript.Ide.Util (discardAnn, identifierFromIdeDeclaration, namespaceForDeclaration, withEmptyAnn)
import Language.PureScript.Ide.Usage (findUsages)
import System.Directory (getCurrentDirectory, getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), normalise)
import Language.PureScript.Names (ModuleName(ModuleName))
import Language.PureScript.AST.SourcePos (SourceSpan(SourceSpan))
import Language.PureScript.Errors (SourcePos(..))
import Database.SQLite.Simple qualified as SQLite
import Language.PureScript (cacheDbFile, runModuleName)
import Debug.Trace qualified as Debug
import Data.Maybe (catMaybes)
import Protolude (head)
import Data.Foldable (find, Foldable (toList, foldMap))
import Data.Text qualified
import Data.Either (isLeft)
import Codec.Serialise (deserialise)
import Data.ByteString.Lazy qualified
import Database.SQLite.Simple (Only(Only))

-- | Accepts a Command and runs it against psc-ide's State. This is the main
-- entry point for the server.
handleCommand
  :: (Ide m, MonadLogger m, MonadError IdeError m)
  => Command
  -> m Success
handleCommand c = case c of
  Load [] ->
    -- Clearing the State before populating it to avoid a space leak
    pure $ TextResult "Done"
    -- resetIdeState *> findAvailableExterns >>= loadModulesAsync
  Load modules ->
    pure $ TextResult "Done"
    -- loadModulesAsync modules
  LoadSync [] ->
    pure $ TextResult "Done"
    -- findAvailableExterns >>= loadModulesSync
  LoadSync modules ->
    pure $ TextResult "Done"
    -- loadModulesSync modules
  Type search filters currentModule ->
    findDeclarations (F.Filter (Right $ F.Exact search) : filters) currentModule Nothing
  Complete filters matcher currentModule complOptions ->
    findDeclarations filters currentModule (Just complOptions)
    -- findCompletions' filters matcher currentModule complOptions
  List LoadedModules -> do
    logWarnN
      "Listing the loaded modules command is DEPRECATED, use the completion command and filter it to modules instead"
    ModuleList . join <$> runQuery "select module_name from modules"
  List AvailableModules ->
    ModuleList . join <$> runQuery "select module_name from modules"
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
  Cwd ->
    TextResult . T.pack <$> liftIO getCurrentDirectory
  Reset ->
    resetIdeState $> TextResult "State has been reset."
  Quit ->
    liftIO exitSuccess

findCompletions
  :: Ide m
  => [F.Filter]
  -> Matcher IdeDeclarationAnn
  -> Maybe P.ModuleName
  -> CompletionOptions
  -> m Success
findCompletions filters matcher currentModule complOptions = do
  modules <- getAllModules currentModule
  let insertPrim = Map.union idePrimDeclarations
  pure (CompletionResult (getCompletions filters matcher complOptions (insertPrim modules)))

findCompletions'
  :: Ide m
  => [F.Filter]
  -> Matcher IdeDeclarationAnn
  -> Maybe P.ModuleName
  -> CompletionOptions
  -> m Success
findCompletions' filters matcher currentModule complOptions = do
  sq <- sqliteFile
  completions <- liftIO $ SQLite.withConnection sq $ \conn -> do
    rows :: [(Text, Text, Maybe Text)] <- SQLite.query conn "select module_name, name, docs from declarations where name glob ?" (SQLite.Only (glob filters :: Text))
    return rows

  pure $ CompletionResult $ completions <&> \(module_name, name, docs) ->
        Completion
          { complModule = module_name
          , complIdentifier = name
          , complType = "TYPE"
          , complExpandedType = "EXPANDED"
          , complLocation = Just (SourceSpan
             { spanName = ".spago/BuildInfo.purs"
             , spanStart = SourcePos
               { sourcePosLine = 3
               , sourcePosColumn = 1
               }
             , spanEnd = SourcePos
               { sourcePosLine = 1
               , sourcePosColumn = 1
               }
             })
          , complDocumentation = docs
          , complExportedFrom =  [ModuleName "BuildInfo"]
          , complDeclarationType = Nothing
          }
  where
  glob :: [F.Filter] -> Text
  glob f = mapMaybe globSearch f & head & fromMaybe "*"
  globSearch :: F.Filter -> Maybe Text
  globSearch (F.Filter (Right (F.Prefix p))) = Just (p <> "*")
  globSearch (F.Filter (Right (F.Exact p))) = Just p
  globSearch _ = Nothing


  -- modules <- getAllModules currentModule
  -- let insertPrim = Map.union idePrimDeclarations
  -- pure (CompletionResult (getCompletions filters matcher complOptions (insertPrim modules)))

findDeclarations
  :: Ide m
  => [F.Filter]
  -> Maybe P.ModuleName
  -> Maybe CompletionOptions
  -> m Success
findDeclarations filters currentModule completionOptions = do
  rows <- runQuery $
    "select module_name, name, type, span " <>
    "from declarations where " <>
    T.intercalate " and " (
      mapMaybe (\case
        F.Filter (Left modules) ->
          Just $ "module_name in (" <> T.intercalate "," (toList modules <&> runModuleName <&> \m -> "'" <> m <> "'") <> ")"
        F.Filter (Right (F.Exact f)) -> Just $ "name glob '" <> f <> "'"
        F.Filter (Right (F.Prefix f)) -> Just $ "name glob '" <> f <> "*'"
        F.Filter _ -> Nothing)
      filters) <>
    foldMap (\maxResults -> " limit " <> show maxResults ) (coMaxResults =<< completionOptions)

  Debug.traceM $ show rows

  pure $ CompletionResult (rows <&> \(module_name, name, type_, span) -> Completion
       { complModule = module_name
       , complIdentifier = name
       , complType = "TYPE"
       , complExpandedType = "EXPANDED"
       , complLocation = deserialise span
       , complDocumentation = type_
       , complExportedFrom =  [ModuleName "MODDD"]
       , complDeclarationType = Nothing
       }
       )

sqliteFile :: Ide m => m FilePath
sqliteFile = outputDirectory <&> ( </> "cache.db")

outputDirectory :: Ide m => m FilePath
outputDirectory = do
  outputPath <- confOutputPath . ideConfiguration <$> ask
  cwd <- liftIO getCurrentDirectory
  pure (cwd </> outputPath)

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
  -- We resolve all the modulenames to externs files and load these into memory.
  oDir <- outputDirectory
  let efPaths =
        map (\mn -> oDir </> toS (P.runModuleName mn) </> P.externsFileName) moduleNames
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

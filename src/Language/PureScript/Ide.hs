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

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}

module Language.PureScript.Ide
       ( handleCommand
         -- for tests
       , printModules
       ) where

import           Prelude                            ()
import           Prelude.Compat

import           Control.Concurrent.Async
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Foldable
import           Data.Maybe                         (catMaybes)
import           Data.Monoid
import           Data.Text                          (Text)
import qualified Data.Text                          as T
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
import           System.Directory
import           System.Exit
import           System.FilePath

handleCommand :: (Ide m, MonadLogger m, MonadError PscIdeError m) =>
                 Command -> m Success
handleCommand (Load []) = loadAllModules
handleCommand (Load modules) = loadModules modules
handleCommand (Type search filters currentModule) =
  findType search filters currentModule
handleCommand (Complete filters matcher currentModule) =
  findCompletions filters matcher currentModule
handleCommand (Pursuit query Package) =
  findPursuitPackages query
handleCommand (Pursuit query Identifier) =
  findPursuitCompletions query
handleCommand (List LoadedModules) =
  printModules
handleCommand (List AvailableModules) =
  listAvailableModules
handleCommand (List (Imports fp)) =
  importsForFile fp
handleCommand (CaseSplit l b e wca t) =
  caseSplit l b e wca t
handleCommand (AddClause l wca) =
  addClause l wca
handleCommand (Import fp outfp _ (AddImplicitImport mn)) = do
  rs <- addImplicitImport fp mn
  answerRequest outfp rs
handleCommand (Import fp outfp filters (AddImportForIdentifier ident)) = do
  rs <- addImportForIdentifier fp ident filters
  case rs of
    Right rs' -> answerRequest outfp rs'
    Left question -> pure $ CompletionResult (map completionFromMatch question)
handleCommand (Rebuild file) =
  rebuildFile file
handleCommand Cwd =
  TextResult . T.pack <$> liftIO getCurrentDirectory
handleCommand Reset = resetIdeState *> pure (TextResult "State has been reset.")
handleCommand Quit = liftIO exitSuccess

findCompletions :: (Ide m) =>
                   [Filter] -> Matcher -> Maybe P.ModuleName -> m Success
findCompletions filters matcher currentModule = do
  modules <- getAllModules2 currentModule
  pure . CompletionResult . map completionFromMatch . getCompletions filters matcher $ modules

findType :: (Ide m) =>
            Text -> [Filter] -> Maybe P.ModuleName -> m Success
findType search filters currentModule = do
  modules <- getAllModules2 currentModule
  pure . CompletionResult . map completionFromMatch . getExactMatches search filters $ modules

findPursuitCompletions :: (MonadIO m) =>
                          PursuitQuery -> m Success
findPursuitCompletions (PursuitQuery q) =
  PursuitResult <$> liftIO (searchPursuitForDeclarations q)

findPursuitPackages :: (MonadIO m) =>
                       PursuitQuery -> m Success
findPursuitPackages (PursuitQuery q) =
  PursuitResult <$> liftIO (findPackagesForModuleIdent q)

printModules :: (Ide m) => m Success
printModules = ModuleList . map runModuleNameT <$> getLoadedModulenames

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
    return (ModuleList (map T.pack cleaned))

caseSplit :: (Ide m, MonadError PscIdeError m) =>
  Text -> Int -> Int -> CS.WildcardAnnotations -> Text -> m Success
caseSplit l b e csa t = do
  patterns <- CS.makePattern l b e csa <$> CS.caseSplit t
  pure (MultilineTextResult patterns)

addClause
  :: (MonadError PscIdeError m)
  => Text
  -> CS.WildcardAnnotations
  -> m Success
addClause t wca = MultilineTextResult <$> CS.addClause t wca

importsForFile :: (MonadIO m, MonadError PscIdeError m) =>
                  FilePath -> m Success
importsForFile fp = do
  imports <- getImportsForFile fp
  pure (ImportList imports)

-- | Takes the output directory and a filepath like "Monad.Control.Eff" and
-- looks up, whether that folder contains an externs.json
checkExternsPath :: FilePath -> FilePath -> IO (Maybe FilePath)
checkExternsPath oDir d
  | d `elem` [".", ".."] = pure Nothing
  | otherwise = do
      let file = oDir </> d </> "externs.json"
      ex <- doesFileExist file
      if ex
        then pure (Just file)
        else pure Nothing

findAllExterns :: (Ide m, MonadError PscIdeError m) => m [FilePath]
findAllExterns = do
  oDir <- outputDirectory
  unlessM (liftIO (doesDirectoryExist oDir))
    (throwError (GeneralError "Couldn't locate your output directory."))
  liftIO $ do
    dirs <- getDirectoryContents oDir
    externPaths <- traverse (checkExternsPath oDir) dirs
    pure (catMaybes externPaths)

loadModules
  :: (Ide m, MonadError PscIdeError m, MonadLogger m)
  => [P.ModuleName]
  -> m Success
loadModules mns = do
  oDir <- outputDirectory
  let efPaths = map (\mn -> oDir </> P.runModuleName mn </> "externs.json") mns
  efiles <- traverse readExternFile efPaths
  traverse_ insertExterns efiles
  --TODO Get rid of this once ModuleOld is gone
  traverse_ insertModule efiles
  populateStage2
  pure (TextResult ("Loaded " <> foldMap runModuleNameT mns <> "."))

loadAllModules :: (Ide m, MonadError PscIdeError m) => m Success
loadAllModules = do
  exts <- traverse readExternFile =<< findAllExterns
  traverse_ insertExterns exts
  --TODO Get rid of this once ModuleOld is gone
  traverse_ insertModule exts
  env <- ask
  _ <- liftIO $ async (runStdoutLoggingT (runReaderT populateStage2 env))
  pure (TextResult "All modules loaded.")

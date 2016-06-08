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
{-# LANGUAGE TemplateHaskell       #-}

module Language.PureScript.Ide
       ( handleCommand
         -- for tests
       , printModules
       ) where

import           Prelude                            ()
import           Prelude.Compat

import           Control.Monad                      (unless)
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Reader.Class
import           Data.Foldable
import qualified Data.Map.Lazy                      as M
import           Data.Maybe                         (catMaybes, mapMaybe)
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
import           Language.PureScript.Ide.Reexports
import           Language.PureScript.Ide.SourceFile
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import           System.Directory
import           System.Exit
import           System.FilePath

handleCommand :: (PscIde m, MonadLogger m, MonadError PscIdeError m) =>
                 Command -> m Success
handleCommand (Load [] []) = loadAllModules
handleCommand (Load modules deps) =
  loadModulesAndDeps modules deps
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
  pure $ addClause l wca
handleCommand (Import fp outfp _ (AddImplicitImport mn)) = do
  rs <- addImplicitImport fp mn
  answerRequest outfp rs
handleCommand (Import fp outfp filters (AddImportForIdentifier ident)) = do
  rs <- addImportForIdentifier fp ident filters
  case rs of
    Right rs' -> answerRequest outfp rs'
    Left question -> pure $ CompletionResult (mapMaybe completionFromMatch question)
handleCommand (Rebuild file) =
  rebuildFile file
handleCommand Cwd =
  TextResult . T.pack <$> liftIO getCurrentDirectory
handleCommand Reset = resetPscIdeState *> pure (TextResult "State has been reset.")
handleCommand Quit = liftIO exitSuccess

findCompletions :: (PscIde m) =>
                   [Filter] -> Matcher -> Maybe P.ModuleName -> m Success
findCompletions filters matcher currentModule = do
  modules <- getAllModulesWithReexportsAndCache currentModule
  pure . CompletionResult . mapMaybe completionFromMatch . getCompletions filters matcher $ modules

findType :: (PscIde m) =>
            DeclIdent -> [Filter] -> Maybe P.ModuleName -> m Success
findType search filters currentModule = do
  modules <- getAllModulesWithReexportsAndCache currentModule
  pure . CompletionResult . mapMaybe completionFromMatch . getExactMatches search filters $ modules

findPursuitCompletions :: (MonadIO m) =>
                          PursuitQuery -> m Success
findPursuitCompletions (PursuitQuery q) =
  PursuitResult <$> liftIO (searchPursuitForDeclarations q)

findPursuitPackages :: (MonadIO m) =>
                       PursuitQuery -> m Success
findPursuitPackages (PursuitQuery q) =
  PursuitResult <$> liftIO (findPackagesForModuleIdent q)

loadExtern :: (PscIde m, MonadLogger m, MonadError PscIdeError m) =>
             FilePath -> m ()
loadExtern fp = do
  m <- readExternFile fp
  insertModule m

printModules :: (PscIde m) => m Success
printModules = printModules' . pscIdeStateModules <$> getPscIdeState

printModules' :: M.Map ModuleIdent [ExternDecl] -> Success
printModules' = ModuleList . M.keys

listAvailableModules :: PscIde m => m Success
listAvailableModules = do
  outputPath <- confOutputPath . envConfiguration <$> ask
  liftIO $ do
    cwd <- getCurrentDirectory
    dirs <- getDirectoryContents (cwd </> outputPath)
    return (ModuleList (listAvailableModules' dirs))

listAvailableModules' :: [FilePath] -> [Text]
listAvailableModules' dirs =
  let cleanedModules = filter (`notElem` [".", ".."]) dirs
  in map T.pack cleanedModules

caseSplit :: (PscIde m, MonadError PscIdeError m) =>
  Text -> Int -> Int -> CS.WildcardAnnotations -> Text -> m Success
caseSplit l b e csa t = do
  patterns <- CS.makePattern l b e csa <$> CS.caseSplit t
  pure (MultilineTextResult patterns)

addClause :: Text -> CS.WildcardAnnotations -> Success
addClause t wca = MultilineTextResult (CS.addClause t wca)

importsForFile :: (MonadIO m, MonadError PscIdeError m) =>
                  FilePath -> m Success
importsForFile fp = do
  imports <- getImportsForFile fp
  pure (ImportList imports)

-- | The first argument is a set of modules to load. The second argument
--   denotes modules for which to load dependencies
loadModulesAndDeps :: (PscIde m, MonadLogger m, MonadError PscIdeError m) =>
                     [ModuleIdent] -> [ModuleIdent] -> m Success
loadModulesAndDeps mods deps = do
  r1 <- mapM loadModule (mods ++ deps)
  r2 <- mapM loadModuleDependencies deps
  let moduleResults = T.concat r1
  let dependencyResults = T.concat r2
  pure (TextResult (moduleResults <> ", " <> dependencyResults))

loadModuleDependencies ::(PscIde m, MonadLogger m, MonadError PscIdeError m) =>
                         ModuleIdent -> m Text
loadModuleDependencies moduleName = do
  m <- getModule moduleName
  case getDependenciesForModule <$> m of
    Just deps -> do
      mapM_ loadModule deps
      -- We need to load the modules, that get reexported from the dependencies
      depModules <- catMaybes <$> mapM getModule deps
      -- What to do with errors here? This basically means a reexported dependency
      -- doesn't exist in the output/ folder
      traverse_ loadReexports depModules
      pure ("Dependencies for " <> moduleName <> " loaded.")
    Nothing -> throwError (ModuleNotFound moduleName)

loadReexports :: (PscIde m, MonadLogger m, MonadError PscIdeError m) =>
                Module -> m [ModuleIdent]
loadReexports m = case getReexports m of
  [] -> pure []
  exportDeps -> do
    -- I'm fine with this crashing on a failed pattern match.
    -- If this ever fails I'll need to look at GADTs
    let reexports = map (\(Export mn) -> mn) exportDeps
    $(logDebug) ("Loading reexports for module: " <> fst m <>
                 " reexports: " <> T.intercalate ", " reexports)
    traverse_ loadModule reexports
    exportDepsModules <- catMaybes <$> traverse getModule reexports
    exportDepDeps <- traverse loadReexports exportDepsModules
    return $ concat exportDepDeps

getDependenciesForModule :: Module -> [ModuleIdent]
getDependenciesForModule (_, decls) = mapMaybe getDependencyName decls
  where getDependencyName (Dependency dependencyName _ _) = Just dependencyName
        getDependencyName _ = Nothing

loadModule :: (PscIde m, MonadLogger m, MonadError PscIdeError m) =>
              ModuleIdent -> m Text
loadModule "Prim" = pure "Prim won't be loaded"
loadModule mn = do
  path <- filePathFromModule mn
  loadExtern path
  $(logDebug) ("Loaded extern file at: " <> T.pack path)
  pure ("Loaded extern file at: " <> T.pack path)

loadAllModules :: (PscIde m, MonadLogger m, MonadError PscIdeError m) => m Success
loadAllModules = do
  outputPath <- confOutputPath . envConfiguration <$> ask
  cwd <- liftIO getCurrentDirectory
  let outputDirectory = cwd </> outputPath
  liftIO (doesDirectoryExist outputDirectory)
    >>= flip unless (throwError (GeneralError "Couldn't locate your output directory"))
  liftIO (getDirectoryContents outputDirectory)
    >>= liftIO . traverse (getExternsPath outputDirectory)
    >>= traverse_ loadExtern . catMaybes
  pure (TextResult "All modules loaded.")
  where
    getExternsPath :: FilePath -> FilePath -> IO (Maybe FilePath)
    getExternsPath outputDirectory d
      | d `elem` [".", ".."] = pure Nothing
      | otherwise = do
          let file = outputDirectory </> d </> "externs.json"
          ex <- doesFileExist file
          if ex
            then pure (Just file)
            else pure Nothing

filePathFromModule :: (PscIde m, MonadError PscIdeError m) =>
                      ModuleIdent -> m FilePath
filePathFromModule moduleName = do
  outputPath <- confOutputPath . envConfiguration <$> ask
  cwd <- liftIO getCurrentDirectory
  let path = cwd </> outputPath </> T.unpack moduleName </> "externs.json"
  ex <- liftIO $ doesFileExist path
  if ex
    then pure path
    else throwError (ModuleFileNotFound moduleName)

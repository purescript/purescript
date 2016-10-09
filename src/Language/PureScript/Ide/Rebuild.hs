{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Language.PureScript.Ide.Rebuild
  ( rebuildFile
  ) where

import           Protolude

import           "monad-logger" Control.Monad.Logger
import qualified Data.List                       as List
import qualified Data.Map.Lazy                   as M
import           Data.Maybe                      (fromJust)
import qualified Data.Set                        as S
import qualified Language.PureScript             as P
import           Language.PureScript.Errors.JSON
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import           System.IO.UTF8                  (readUTF8File)

-- | Given a filepath performs the following steps:
--
-- * Reads and parses a PureScript module from the filepath.
--
-- * Builds a dependency graph for the parsed module from the already loaded
-- ExternsFiles.
--
-- * Attempts to find an FFI definition file for the module by looking
-- for a file with the same filepath except for a .js extension.
--
-- * Passes all the created artifacts to @rebuildModule@.
--
-- * If the rebuilding succeeds, returns a @RebuildSuccess@ with the generated
-- warnings, and if rebuilding fails, returns a @RebuildError@ with the
-- generated errors.
rebuildFile
  :: (Ide m, MonadLogger m, MonadError PscIdeError m)
  => FilePath
  -> m Success
rebuildFile path = do

  input <- liftIO (readUTF8File path)

  m <- case snd <$> P.parseModuleFromFile identity (path, input) of
    Left parseError -> throwError
                       . RebuildError
                       . toJSONErrors False P.Error
                       $ P.MultipleErrors [P.toPositionedError parseError]
    Right m -> pure m

  -- Externs files must be sorted ahead of time, so that they get applied
  -- correctly to the 'Environment'.
  externs <- sortExterns m =<< getExternFiles

  outputDirectory <- confOutputPath . ideConfiguration <$> ask

  -- For rebuilding, we want to 'RebuildAlways', but for inferring foreign
  -- modules using their file paths, we need to specify the path in the 'Map'.
  let filePathMap = M.singleton (P.getModuleName m) (Left P.RebuildAlways)
  foreigns <- P.inferForeignModules (M.singleton (P.getModuleName m) (Right path))

  let makeEnv = MakeActionsEnv outputDirectory filePathMap foreigns False
  -- Rebuild the single module using the cached externs
  (result, warnings) <- liftIO
    . P.runMake P.defaultOptions
    . P.rebuildModule (buildMakeActions
                        >>= shushProgress $ makeEnv) externs $ m
  case result of
    Left errors -> throwError (RebuildError (toJSONErrors False P.Error errors))
    Right _ -> do
      rebuildModuleOpen makeEnv externs m
      pure (RebuildSuccess (toJSONErrors False P.Warning warnings))

-- | Rebuilds a module but opens up its export list first and stores the result
-- inside the rebuild cache
rebuildModuleOpen
  :: (Ide m, MonadLogger m, MonadError PscIdeError m)
  => MakeActionsEnv
  -> [P.ExternsFile]
  -> P.Module
  -> m ()
rebuildModuleOpen makeEnv externs m = do
  (openResult, _) <- liftIO
    . P.runMake P.defaultOptions
    . P.rebuildModule (buildMakeActions
                       >>= shushProgress
                       >>= shushCodegen
                       $ makeEnv) externs $ openModuleExports m
  case openResult of
    Left _ ->
      throwError (GeneralError "Failed when rebuilding with open exports")
    Right result -> do
      $(logDebug)
        ("Setting Rebuild cache: " <> runModuleNameT (P.efModuleName result))
      cacheRebuild result

-- | Parameters we can access while building our @MakeActions@
data MakeActionsEnv =
  MakeActionsEnv
  { maeOutputDirectory :: FilePath
  , maeFilePathMap     :: Map P.ModuleName (Either P.RebuildPolicy FilePath)
  , maeForeignPathMap  :: Map P.ModuleName FilePath
  , maePrefixComment   :: Bool
  }

-- | Builds the default @MakeActions@ from a @MakeActionsEnv@
buildMakeActions :: MakeActionsEnv -> P.MakeActions P.Make
buildMakeActions MakeActionsEnv{..} =
  P.buildMakeActions
    maeOutputDirectory
    maeFilePathMap
    maeForeignPathMap
    maePrefixComment

-- | Shuts the compiler up about progress messages
shushProgress :: P.MakeActions P.Make -> MakeActionsEnv -> P.MakeActions P.Make
shushProgress ma _ =
  ma { P.progress = \_ -> pure () }

-- | Stops any kind of codegen (also silences errors about missing or unused FFI
-- files though)
shushCodegen :: P.MakeActions P.Make -> MakeActionsEnv -> P.MakeActions P.Make
shushCodegen ma MakeActionsEnv{..} =
  ma { P.codegen = \_ _ _ -> pure () }

-- | Returns a topologically sorted list of dependent ExternsFiles for the given
-- module. Throws an error if there is a cyclic dependency within the
-- ExternsFiles
sortExterns
  :: (Ide m, MonadError PscIdeError m)
  => P.Module
  -> Map P.ModuleName P.ExternsFile
  -> m [P.ExternsFile]
sortExterns m ex = do
  sorted' <- runExceptT
           . P.sortModules
           . (:) m
           . map mkShallowModule
           . M.elems
           . M.delete (P.getModuleName m) $ ex
  case sorted' of
    Left err ->
      throwError (RebuildError (toJSONErrors False P.Error err))
    Right (sorted, graph) -> do
      let deps = fromJust (List.lookup (P.getModuleName m) graph)
      pure $ mapMaybe getExtern (deps `inOrderOf` map P.getModuleName sorted)
  where
    mkShallowModule P.ExternsFile{..} =
      P.Module (P.internalModuleSourceSpan "<rebuild>") [] efModuleName (map mkImport efImports) Nothing
    mkImport (P.ExternsImport mn it iq) =
      P.ImportDeclaration mn it iq
    getExtern mn = M.lookup mn ex
    -- Sort a list so its elements appear in the same order as in another list.
    inOrderOf :: (Ord a) => [a] -> [a] -> [a]
    inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

-- | Removes a modules export list.
openModuleExports :: P.Module -> P.Module
openModuleExports (P.Module ss cs mn decls _) = P.Module ss cs mn decls Nothing

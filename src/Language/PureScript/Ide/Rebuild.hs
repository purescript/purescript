{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Language.PureScript.Ide.Rebuild where

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import qualified Data.Map.Lazy                   as M
import           Data.Maybe                      (fromJust, mapMaybe)
import qualified Data.Set                        as S
import qualified Language.PureScript             as P
import           Language.PureScript.Errors.JSON
import qualified Language.PureScript.Externs     as P
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types
import           System.Directory                (doesFileExist)
import           System.FilePath                 (replaceExtension)
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
-- * Adds a default import for Prim to the parsed module and passes all the
-- created artifacts to @rebuildModule@.
--
-- * If the rebuilding succeeds, returns a @RebuildSuccess@ with the generated
-- warnings, and if rebuilding fails, returns a @RebuildError@ with the
-- generated errors.
rebuildFile
  :: (PscIde m, MonadLogger m, MonadError PscIdeError m)
  => FilePath
  -> m Success
rebuildFile path = do

  input <- liftIO $ readUTF8File path

  m <- case map snd <$> P.parseModulesFromFiles id [(path, input)] of
         Left parseError ->
           throwError . RebuildError . toJSONErrors False P.Error $ parseError
         Right [m] -> pure m
         Right _ -> throwError . GeneralError $ "Please define exactly one module."

  externs <- sortExterns m =<< getExternFiles

  outputDirectory <- confOutputPath . envConfiguration <$> ask

  let foreignModule = replaceExtension path "js"
  foreignExists <- liftIO (doesFileExist foreignModule)


  let mbe = MakeActionsEnv
        { mbeOutputDirectory = outputDirectory
        , mbeFilePathMap = M.singleton (P.getModuleName m) (Left P.RebuildAlways)
        , mbeForeignPathMap = if foreignExists
                              then M.singleton (P.getModuleName m) foreignModule
                              else M.empty
        , mbePrefixComment = False
        }
  (result, warnings) <- liftIO
                   . P.runMake P.defaultOptions
                   . P.rebuildModule (buildMakeActions >>= shushProgress $ mbe) externs
                   $ P.addDefaultImport (P.ModuleName [P.ProperName "Prim"]) m
  case result of
    Left errors -> throwError . RebuildError $ toJSONErrors False P.Error errors
    Right ef -> do
      setCachedRebuild ef
      pure . RebuildSuccess $ toJSONErrors False P.Warning warnings

-- | Parameters we can access while building our @MakeActions@
data MakeActionsEnv =
  MakeActionsEnv
  { mbeOutputDirectory :: FilePath
  , mbeFilePathMap     :: M.Map P.ModuleName (Either P.RebuildPolicy FilePath)
  , mbeForeignPathMap  :: M.Map P.ModuleName FilePath
  , mbePrefixComment   :: Bool
  }

-- | Builds the default @MakeActions@ from a @MakeActionsEnv@
buildMakeActions :: MakeActionsEnv -> P.MakeActions P.Make
buildMakeActions MakeActionsEnv{..} =
  P.buildMakeActions mbeOutputDirectory mbeFilePathMap mbeForeignPathMap mbePrefixComment

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
  :: (PscIde m, MonadError PscIdeError m)
  => P.Module
  -> M.Map P.ModuleName P.ExternsFile
  -> m [P.ExternsFile]
sortExterns m ex = do
  sorted' <- runExceptT
           . P.sortModules
           . (:) m
           . map mkShallowModule
           . M.elems
           . M.delete (P.getModuleName m) $ ex
  case sorted' of
    Left _ -> throwError (GeneralError "There was a cycle in the dependencies")
    Right (sorted, graph) -> do
      let deps = fromJust (lookup (P.getModuleName m) graph)
      pure $ mapMaybe getExtern (deps `inOrderOf` map P.getModuleName sorted)
  where
    mkShallowModule P.ExternsFile{..} =
      P.Module undefined [] efModuleName (map mkImport efImports) Nothing
    mkImport (P.ExternsImport mn it iq) =
      P.ImportDeclaration mn it iq
    getExtern mn = M.lookup mn ex
    -- Sort a list so its elements appear in the same order as in another list.
    inOrderOf :: (Ord a) => [a] -> [a] -> [a]
    inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

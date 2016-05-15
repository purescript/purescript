{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}

module Language.PureScript.Ide.Rebuild where

import           Prelude.Compat

import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Except
import qualified Data.Map.Lazy                   as M
import           Data.Maybe                      (fromJust, mapMaybe)
import qualified Data.Set                        as S
import qualified Language.PureScript             as P
import           Language.PureScript.Errors.JSON
import           System.IO.UTF8 (readUTF8File)

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

  -- Externs files must be sorted ahead of time, so that they get applied
  -- correctly to the 'Environment'.
  externs <- sortExterns m . M.delete (P.getModuleName m) =<< getExternFiles

  outputDirectory <- confOutputPath . envConfiguration <$> ask

  -- For rebuilding, we want to 'RebuildAlways', but for inferring foreign
  -- modules using their file paths, we need to specify the path in the 'Map'.
  let filePathMap = M.singleton (P.getModuleName m) (Left P.RebuildAlways)
  foreigns <- P.inferForeignModules (M.singleton (P.getModuleName m) (Right path))

  -- Silence progress update messages during the build
  let actions = (P.buildMakeActions outputDirectory filePathMap foreigns False)
                  { P.progress = const (pure ()) }

  -- Rebuild the single module using the cached externs
  (result, warnings) <- liftIO
                        . P.runMake P.defaultOptions
                        . P.rebuildModule actions externs
                        $ m
  case result of
    Left errors -> throwError . RebuildError $ toJSONErrors False P.Error errors
    Right _ -> pure . RebuildSuccess $ toJSONErrors False P.Warning warnings

sortExterns
  :: (PscIde m, MonadError PscIdeError m)
  => P.Module
  -> M.Map P.ModuleName P.ExternsFile
  -> m [P.ExternsFile]
sortExterns m ex = do
  sorted' <- runExceptT . P.sortModules . (:) m . map mkShallowModule . M.elems $ ex
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

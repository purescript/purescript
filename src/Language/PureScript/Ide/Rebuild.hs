{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Language.PureScript.Ide.Rebuild where

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
import qualified Language.PureScript.Externs     as P
import           System.FilePath (replaceExtension)
import           System.Directory (doesFileExist)
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

  externs <- sortExterns m . M.delete (P.getModuleName m) =<< getExternFiles

  outputDirectory <- confOutputPath . envConfiguration <$> ask

  let foreignModule = replaceExtension path "js"
  foreignExists <- liftIO (doesFileExist foreignModule)

  let ma = P.buildMakeActions outputDirectory
                              (M.singleton (P.getModuleName m) (Left P.RebuildAlways))
                              (if foreignExists
                                 then M.singleton (P.getModuleName m) foreignModule
                                 else M.empty)
                              False
  (result, warnings) <- liftIO
                   . P.runMake P.defaultOptions
                   . P.rebuildModule (ma { P.progress = const (pure ()) }) externs
                   $ P.addDefaultImport (P.ModuleName [P.ProperName "Prim"]) m
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
      P.ImportDeclaration mn it iq False
    getExtern mn = M.lookup mn ex
    -- Sort a list so its elements appear in the same order as in another list.
    inOrderOf :: (Ord a) => [a] -> [a] -> [a]
    inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

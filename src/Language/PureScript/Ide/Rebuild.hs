{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE RecordWildCards       #-}

module Language.PureScript.Ide.Rebuild where

import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Error

import           Control.Monad.Error.Class
import           Control.Monad.Reader.Class
import qualified Data.Map.Lazy                      as M
import           Data.Maybe (mapMaybe)
import           Control.Monad.IO.Class
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Trans.Except
import qualified Language.PureScript as P
import qualified Language.PureScript.Externs as P
import           Language.PureScript.Errors.JSON

rebuildFile
  :: (PscIde m, MonadLogger m, MonadError PscIdeError m)
  => FilePath
  -> m Success
rebuildFile path = do

  input <- liftIO $ readFile path

  m <- case map snd <$> P.parseModulesFromFiles id [(path, input)] of
         Left parseError ->
           throwError . GeneralError $ P.prettyPrintMultipleErrors False parseError
         Right [m] -> pure m
         Right _ -> throwError . GeneralError $ "Please define exactly one module."

  let externFilter mn _ = mn /= P.getModuleName m

  externs <- sortExterns . M.filterWithKey externFilter =<< getExternFiles

  outputDirectory <- confOutputPath . envConfiguration <$> ask

  let ma = P.buildMakeActions outputDirectory
                              (M.singleton (P.getModuleName m) (Left P.RebuildAlways))
                              M.empty {- TODO: add foreign module here, if it exists -}
                              False

  (_, warnings) <- liftIO
                   . P.runMake P.defaultOptions
                   . P.rebuildModule ma externs
                   $ m

  pure . RebuildSuccess $ toJSONErrors False P.Warning warnings

sortExterns
  :: (PscIde m, MonadError PscIdeError m)
  => M.Map P.ModuleName P.ExternsFile
  -> m [P.ExternsFile]
sortExterns ex = do
  sorted' <- runExceptT . P.sortModules . map mkShallowModule .M.elems $ ex
  case sorted' of
    Left _ -> throwError (GeneralError "There was a cycle in the dependencies")
    Right (sorted, _) ->
       pure $ mapMaybe getExtern sorted
  where
    mkShallowModule P.ExternsFile{..} =
      P.Module undefined [] efModuleName (map mkImport efImports) Nothing
    mkImport (P.ExternsImport mn it iq) =
      P.ImportDeclaration mn it iq False
    getExtern m = M.lookup (P.getModuleName m) ex

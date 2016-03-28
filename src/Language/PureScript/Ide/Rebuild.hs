{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE TupleSections         #-}

module Language.PureScript.Ide.Rebuild where

import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Error

import           Control.Monad (unless)
import           Control.Monad.Error.Class
import           Control.Monad.Reader.Class
import           Data.Foldable
import qualified Data.Map.Lazy                      as M
import           Data.Maybe (fromMaybe)
import           Control.Monad.IO.Class
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader (runReaderT)
import qualified Control.Monad.Logger as Logger
import qualified Language.PureScript as P
import qualified Language.PureScript.Externs as P
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.CodeGen.JS as J

import           System.FilePath

rebuildFile
  :: (PscIde m, MonadLogger m, MonadError PscIdeError m)
  => FilePath
  -> Maybe FilePath
  -> m Success
rebuildFile path outpath = do
  externs <- M.elems <$> getExternFiles

  outputDirectory <- confOutputPath . envConfiguration <$> ask

  let initEnv = foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment externs

  input <- liftIO $ readFile path

  _ <- case map snd <$> P.parseModulesFromFiles id [(path, input)] of
         Left parseError ->
           throwError . GeneralError $ P.prettyPrintMultipleErrors False parseError
         Right [m] -> do
           (resultMay, _) <- liftIO . Logger.runLogger' . runExceptT . flip runReaderT P.defaultOptions $ do
             ((P.Module ss coms moduleName elaborated exps, env), nextVar) <- P.runSupplyT 0 $ do
               [desugared] <- P.desugar externs [ P.addDefaultImport (P.ModuleName [P.ProperName "Prim"]) m ]
               P.runCheck' initEnv $ P.typeCheckModule desugared
             regrouped <- P.createBindingGroups moduleName . P.collapseBindingGroups $ elaborated
             let mod' = P.Module ss coms moduleName regrouped exps
                 corefn = CF.moduleToCoreFn env mod'
                 [renamed] = P.renameInModules [corefn]
             unless (null . CF.moduleForeign $ renamed)
               . throwError
               . P.errorMessage
               $ P.MissingFFIModule moduleName
             P.evalSupplyT nextVar $ P.prettyPrintJS <$> J.moduleToJs renamed Nothing
           case resultMay of
             Left errs ->
               throwError . GeneralError . P.prettyPrintMultipleErrors False $ errs
             Right js -> do
               let jsPath = fromMaybe
                     (outputDirectory </> P.runModuleName (P.getModuleName m) </> "index.js") outpath
               liftIO $ writeFile jsPath js
         Right _ -> throwError . GeneralError $ "Please define exactly one module."

  return $ TextResult "OK"

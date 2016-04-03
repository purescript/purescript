-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.State
-- Description : Functions to access psc-ide's state
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Functions to access psc-ide's state
-----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Language.PureScript.Ide.State where

import           Prelude                           ()
import           Prelude.Compat

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Reader.Class
import qualified Data.Map.Lazy                     as M
import           Data.Maybe                        (catMaybes)
import           Data.Monoid
import qualified Data.Text                         as T
import           Language.PureScript.Externs
import           Language.PureScript.Ide.Externs
import           Language.PureScript.Ide.Reexports
import           Language.PureScript.Ide.Types
import           Language.PureScript.Names

getPscIdeState :: (PscIde m) =>
                  m (M.Map ModuleIdent [ExternDecl])
getPscIdeState = do
  stateVar <- envStateVar <$> ask
  liftIO $ pscStateModules <$> readTVarIO stateVar

getExternFiles :: (PscIde m) =>
                  m (M.Map ModuleName ExternsFile)
getExternFiles = do
  stateVar <- envStateVar <$> ask
  liftIO (externsFiles <$> readTVarIO stateVar)

getAllDecls :: (PscIde m) => m [ExternDecl]
getAllDecls = concat <$> getPscIdeState

getAllModules :: (PscIde m) => m [Module]
getAllModules = M.toList <$> getPscIdeState

getAllModulesWithReexports :: (PscIde m, MonadLogger m) =>
                              m [Module]
getAllModulesWithReexports = do
  mis <- M.keys <$> getPscIdeState
  ms  <- traverse getModuleWithReexports mis
  pure (catMaybes ms)

getModule :: (PscIde m, MonadLogger m) =>
             ModuleIdent -> m (Maybe Module)
getModule m = do
  modules <- getPscIdeState
  pure ((m,) <$> M.lookup m modules)

getModuleWithReexports :: (PscIde m, MonadLogger m) =>
                          ModuleIdent -> m (Maybe Module)
getModuleWithReexports mi = do
  m <- getModule mi
  modules <- getPscIdeState
  pure $ resolveReexports modules <$> m

insertModule ::(PscIde m, MonadLogger m) =>
               ExternsFile -> m ()
insertModule externsFile = do
  env <- ask
  let moduleName = efModuleName externsFile
  $(logDebug) $ "Inserting Module: " <> T.pack (runModuleName moduleName)
  liftIO . atomically $ insertModule' (envStateVar env) externsFile

insertModule' :: TVar PscIdeState -> ExternsFile -> STM ()
insertModule' st ef =
    modifyTVar st $ \x ->
      x { externsFiles = M.insert (efModuleName ef) ef (externsFiles x)
        , pscStateModules = let (mn, decls) = convertExterns ef
                            in M.insert mn decls (pscStateModules x)
        }

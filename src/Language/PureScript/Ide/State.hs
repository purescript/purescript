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

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE TemplateHaskell       #-}

module Language.PureScript.Ide.State
  ( getPscIdeState
  , getExternFiles
  , getModule
  , getModuleWithReexports
  , getAllModulesWithReexports
  , getAllModulesWithReexportsAndCache
  , insertModule
  , insertModuleSTM
  , getCachedRebuild
  , resetPscIdeState
  , setCachedRebuild
  ) where

import           Prelude                           ()
import           Prelude.Compat

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Reader.Class
import qualified Data.Map.Lazy                     as M
import           Data.Maybe                        (mapMaybe)
import           Data.Monoid
import           Language.PureScript.Externs
import           Language.PureScript.Ide.Externs
import           Language.PureScript.Ide.Reexports
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import qualified Language.PureScript as P

-- | Resets the PscIdeState to emptyPscIdeState
resetPscIdeState :: PscIde m => m ()
resetPscIdeState = do
  stateVar <- envStateVar <$> ask
  liftIO $ atomically (writeTVar stateVar emptyPscIdeState)

-- | Gets the entire PscIdeState
getPscIdeState :: PscIde m => m PscIdeState
getPscIdeState = do
  stateVar <- envStateVar <$> ask
  liftIO (readTVarIO stateVar)

-- | Gets all loaded ExternFiles
getExternFiles :: (PscIde m) => m (M.Map P.ModuleName ExternsFile)
getExternFiles = do
  stateVar <- envStateVar <$> ask
  liftIO (pscIdeStateExternsFiles <$> readTVarIO stateVar)

-- | Gets all loaded Modules and resolves Reexports
getAllModulesWithReexports :: (PscIde m) => m [Module]
getAllModulesWithReexports = getAllModulesWithReexports' <$> getPscIdeState

-- | Pure version of @getAllModulesWithReexports@
getAllModulesWithReexports' :: PscIdeState -> [Module]
getAllModulesWithReexports' state =
  mapMaybe (getModuleWithReexports' state) (M.keys (pscIdeStateModules state))

-- | Checks if the given ModuleName matches the last rebuild cache and if it
-- does, runs @getAllModulesWithReexports@ with the cached module replacing the
-- loaded module
getAllModulesWithReexportsAndCache
  :: (PscIde m)
  => Maybe P.ModuleName
  -> m [Module]
getAllModulesWithReexportsAndCache Nothing = getAllModulesWithReexports
getAllModulesWithReexportsAndCache (Just mn) = do
  state <- getPscIdeState
  cachedRebuild <- getCachedRebuild
  case cachedRebuild of
    Just (cachedIdent, ef) | cachedIdent == mn ->
     pure (getAllModulesWithReexports' (insertModule' ef state))
    _ -> getAllModulesWithReexports

-- | Looks up a single Module inside the loaded Modules
getModule :: (PscIde m) => ModuleIdent -> m (Maybe Module)
getModule m = getModule' <$> getPscIdeState <*> pure m

-- | Pure version of @getModule@
getModule' :: PscIdeState -> ModuleIdent -> Maybe Module
getModule' ps mi = (mi,) <$> M.lookup mi (pscIdeStateModules ps)

-- | Looks up a single Module and resolves its Reexports
getModuleWithReexports :: PscIde m => ModuleIdent -> m (Maybe Module)
getModuleWithReexports i = getModuleWithReexports' <$> getPscIdeState <*> pure i

-- | Pure version of @getModuleWithReexports@
getModuleWithReexports' :: PscIdeState -> ModuleIdent -> Maybe Module
getModuleWithReexports' ps mi =
  resolveReexports (pscIdeStateModules ps) <$> getModule' ps mi

-- | Inserts an @ExternsFile@ into the PscIdeState. Also converts the
-- ExternsFile into psc-ide's internal Declaration format
insertModule :: (PscIde m, MonadLogger m) =>
               ExternsFile -> m ()
insertModule externsFile = do
  stateVar <- envStateVar <$> ask
  let moduleName = efModuleName externsFile
  $(logDebug) $ "Inserting Module: " <> runModuleNameT moduleName
  liftIO . atomically $ insertModuleSTM stateVar externsFile

-- | STM version of insertModule
insertModuleSTM :: TVar PscIdeState -> ExternsFile -> STM ()
insertModuleSTM st ef = modifyTVar st (insertModule' ef)

-- | Pure version of insertModule
insertModule' :: ExternsFile -> PscIdeState -> PscIdeState
insertModule' ef state =
  state
  { pscIdeStateExternsFiles =
      M.insert (efModuleName ef) ef (pscIdeStateExternsFiles state)
  , pscIdeStateModules = let (mn, decls) = convertExterns ef
                         in M.insert mn decls (pscIdeStateModules state)
  }

-- | Sets rebuild cache to the given ExternsFile
setCachedRebuild :: PscIde m => ExternsFile -> m ()
setCachedRebuild ef = do
  st <- envStateVar <$> ask
  liftIO . atomically . modifyTVar st $ \x ->
    x { pscIdeStateCachedRebuild = Just (efModuleName ef, ef) }

-- | Retrieves the rebuild cache
getCachedRebuild :: PscIde m => m (Maybe (P.ModuleName, ExternsFile))
getCachedRebuild = pscIdeStateCachedRebuild <$> getPscIdeState

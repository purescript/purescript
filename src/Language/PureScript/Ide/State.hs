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
  ( getLoadedModulenames
  , getExternFiles
  , insertModule
  , resetIdeState
  , cacheRebuild
  , insertExterns
  , insertExternsSTM
  , getAllModules2
  , getStage1
  , setStage1
  , getStage2
  , setStage2
  , populateStage2
  , populateStage2STM
  ) where

import           Prelude                           ()
import           Prelude.Compat

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Reader.Class
import qualified Data.Map.Lazy                     as M
import           Data.Monoid
import qualified Data.Text                         as T
import           Data.Time                         (getCurrentTime, diffUTCTime)
import           Language.PureScript.Externs
import           Language.PureScript.Ide.Externs
import           Language.PureScript.Ide.Reexports
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import qualified Language.PureScript as P

-- | Resets all State inside psc-ide
resetIdeState :: Ide m => m ()
resetIdeState = do
  stateVar <- envStateVar <$> ask
  ideVar <- ideStateVar <$> ask
  liftIO . atomically $ do
    writeTVar stateVar emptyPscIdeState
    writeTVar ideVar emptyIdeState
    setStage2STM ideVar emptyStage2

-- | Gets the loaded Modulenames
getLoadedModulenames :: Ide m => m [P.ModuleName]
getLoadedModulenames = M.keys <$> getExternFiles

-- | Gets all loaded ExternFiles
getExternFiles :: Ide m => m (M.Map P.ModuleName ExternsFile)
getExternFiles = s1Externs <$> getStage1

-- | Inserts an @ExternsFile@ into the PscIdeState. Also converts the
-- ExternsFile into psc-ide's internal Declaration format
-- TODO: should be removed when the "old" Declaration format gets removed
insertModule :: Ide m => ExternsFile -> m ()
insertModule externsFile = do
  stateVar <- envStateVar <$> ask
  liftIO . atomically $ insertModuleSTM stateVar externsFile

-- | STM version of insertModule
insertModuleSTM :: TVar PscIdeState -> ExternsFile -> STM ()
insertModuleSTM st ef = modifyTVar st (insertModule' ef)

-- | Pure version of insertModule
insertModule' :: ExternsFile -> PscIdeState -> PscIdeState
insertModule' ef state =
  state
  { pscIdeStateModules = let (mn, decls) = convertExterns ef
                         in M.insert mn decls (pscIdeStateModules state)
  }

-- | Retrieves Stage1 from the State.
--  This includes loaded Externfiles
-- (TODO: as soon as we actually parse the modules) aswell as the parsed modules
getStage1 :: Ide m => m Stage1
getStage1 = do
  st <- ideStateVar <$> ask
  fmap ideStage1 . liftIO . readTVarIO $ st

-- | STM version of getStage1
getStage1STM :: TVar IdeState -> STM Stage1
getStage1STM ref = ideStage1 <$> readTVar ref

-- | Sets Stage1 inside the compiler
setStage1 :: Ide m => Stage1 -> m ()
setStage1 s1 = do
  st <- ideStateVar <$> ask
  liftIO . atomically . modifyTVar st $ \x ->
    x {ideStage1 = s1}
  pure ()

-- TODO: Soon to be Stage3
-- | Retrieves Stage2 from the State.
-- This includes the denormalized Declarations and cached rebuilds
getStage2 :: Ide m => m Stage2
getStage2 = do
  st <- ideStateVar <$> ask
  fmap ideStage2 . liftIO . readTVarIO $ st

-- | Sets Stage2 inside the compiler
setStage2 :: Ide m => Stage2 -> m ()
setStage2 s2 = do
  st <- ideStateVar <$> ask
  liftIO . atomically $ setStage2STM st s2

-- | STM version of setStage2
setStage2STM :: TVar IdeState -> Stage2 -> STM ()
setStage2STM ref s2 = do
  modifyTVar ref $ \x ->
    x {ideStage2 = s2}
  pure ()

-- | Checks if the given ModuleName matches the last rebuild cache and if it
-- does returns all loaded definitions + the definitions inside the rebuild
-- cache
getAllModules2 :: Ide m => Maybe P.ModuleName -> m [(P.ModuleName, [IdeDeclaration])]
getAllModules2 mmoduleName = do
  modules <- s2Modules <$> getStage2
  rebuild <- cachedRebuild
  case mmoduleName of
    Nothing -> pure . M.toList $ modules
    Just moduleName ->
      case rebuild of
        Just (cachedModulename, ef)
          | cachedModulename == moduleName ->
            pure . M.toList $
              M.insert moduleName (snd . convertModule . convertExterns $ ef) modules
        _ -> pure . M.toList $ modules

-- | Adds an ExternsFile into psc-ide's State Stage1. This does not populate the
-- following Stages, which needs to be done after all the necessary Exterms have
-- been loaded.
insertExterns :: Ide m => ExternsFile -> m ()
insertExterns ef = do
  st <- ideStateVar <$> ask
  liftIO (atomically (insertExternsSTM st ef))

-- | STM version of insertExterns
insertExternsSTM :: TVar IdeState -> ExternsFile -> STM ()
insertExternsSTM ref ef =
  modifyTVar ref $ \x ->
    x { ideStage1 = (ideStage1 x) {
          s1Externs = M.insert (efModuleName ef) ef (s1Externs (ideStage1 x))}}

-- | Sets rebuild cache to the given ExternsFile
cacheRebuild :: Ide m => ExternsFile -> m ()
cacheRebuild ef = do
  st <- ideStateVar <$> ask
  liftIO . atomically . modifyTVar st $ \x ->
    x { ideStage2 = (ideStage2 x) {
          s2CachedRebuild = Just (efModuleName ef, ef)}}

-- | Retrieves the rebuild cache
cachedRebuild :: Ide m => m (Maybe (P.ModuleName, ExternsFile))
cachedRebuild = s2CachedRebuild <$> getStage2

-- | Resolves reexports and populates Stage2 with data to be used in queries.
populateStage2 :: (Ide m, MonadLogger m) => m ()
populateStage2 = do
  st <- ideStateVar <$> ask
  duration <- liftIO $ do
    start <- getCurrentTime
    atomically (populateStage2STM st)
    end <- getCurrentTime
    pure (diffUTCTime end start)
  $(logDebug) $ "Finished populating Stage2 in " <> T.pack (show duration)

-- | STM version of populateStage2
populateStage2STM :: TVar IdeState -> STM ()
populateStage2STM ref = do
  externs <- s1Externs <$> getStage1STM ref
  -- Build the "old" ExternDecl format
  let modules = M.mapKeys runModuleNameT . M.map (snd . convertExterns) $ externs
      -- Convert ExternDecl into IdeDeclaration
      declarations = resolveReexports2 modules <$> M.toList modules
  setStage2STM ref (Stage2 (M.fromList declarations) Nothing)

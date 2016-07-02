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
  , insertModuleOld
  , resetIdeState
  , cacheRebuild
  , insertExterns
  , insertModule
  , insertModuleSTM
  , insertExternsSTM
  , getAllModules2
  , getStage1
  , setStage1
  , getStage3
  , setStage3
  , populateStage2
  , populateStage3
  , populateStage3STM
  ) where

import           Protolude
import qualified Prelude

import           Control.Concurrent.STM
import           "monad-logger" Control.Monad.Logger
import qualified Data.Map.Lazy                     as M
import           Language.PureScript.Externs
import           Language.PureScript.Ide.Externs
import           Language.PureScript.Ide.Reexports
import           Language.PureScript.Ide.SourceFile
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import qualified Language.PureScript as P
import           System.Clock
import           System.FilePath

-- | Resets all State inside psc-ide
resetIdeState :: Ide m => m ()
resetIdeState = do
  stateVar <- envStateVar <$> ask
  ideVar <- ideStateVar <$> ask
  liftIO . atomically $ do
    writeTVar stateVar emptyPscIdeState
    writeTVar ideVar emptyIdeState
    setStage3STM ideVar emptyStage3

-- | Gets the loaded Modulenames
getLoadedModulenames :: Ide m => m [P.ModuleName]
getLoadedModulenames = M.keys <$> getExternFiles

-- | Gets all loaded ExternFiles
getExternFiles :: Ide m => m (M.Map P.ModuleName ExternsFile)
getExternFiles = s1Externs <$> getStage1

-- | Inserts an @ExternsFile@ into the PscIdeState. Also converts the
-- ExternsFile into psc-ide's internal Declaration format
-- TODO: should be removed when the "old" Declaration format gets removed
insertModuleOld :: Ide m => ExternsFile -> m ()
insertModuleOld externsFile = do
  stateVar <- envStateVar <$> ask
  liftIO . atomically $ insertModuleOldSTM stateVar externsFile

-- | STM version of insertModuleOld
insertModuleOldSTM :: TVar PscIdeState -> ExternsFile -> STM ()
insertModuleOldSTM st ef = modifyTVar st (insertModule' ef)

-- | Pure version of insertModuleOld
insertModule' :: ExternsFile -> PscIdeState -> PscIdeState
insertModule' ef state =
  state
  { pscIdeStateModules = let (mn, decls) = convertExterns ef
                         in M.insert mn decls (pscIdeStateModules state)
  }

-- | Insert a Module into Stage1 of the State
insertModule :: Ide m => (FilePath, P.Module) -> m ()
insertModule module' = do
  stateVar <- ideStateVar <$> ask
  liftIO . atomically $ insertModuleSTM stateVar module'

-- | STM version of insertModule
insertModuleSTM :: TVar IdeState -> (FilePath, P.Module) -> STM ()
insertModuleSTM ref (fp, module') =
  modifyTVar ref $ \x ->
    x { ideStage1 = (ideStage1 x) {
          s1Modules = M.insert
            (P.getModuleName module')
            (module', fp)
            (s1Modules (ideStage1 x))}}

-- | Retrieves Stage1 from the State.
--  This includes loaded Externfiles
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

-- | Retrieves Stage2 from the State.
getStage2 :: Ide m => m Stage2
getStage2 = do
  st <- ideStateVar <$> ask
  liftIO (atomically (getStage2STM st))

getStage2STM ::  TVar IdeState -> STM Stage2
getStage2STM ref = ideStage2 <$> readTVar ref

-- | STM version of setStage2
setStage2STM :: TVar IdeState -> Stage2 -> STM ()
setStage2STM ref s2 = do
  modifyTVar ref $ \x ->
    x {ideStage2 = s2}
  pure ()

-- | Retrieves Stage3 from the State.
-- This includes the denormalized Declarations and cached rebuilds
getStage3 :: Ide m => m Stage3
getStage3 = do
  st <- ideStateVar <$> ask
  fmap ideStage3 . liftIO . readTVarIO $ st

-- | Sets Stage3 inside the compiler
setStage3 :: Ide m => Stage3 -> m ()
setStage3 s3 = do
  st <- ideStateVar <$> ask
  liftIO . atomically $ setStage3STM st s3

-- | STM version of setStage3
setStage3STM :: TVar IdeState -> Stage3 -> STM ()
setStage3STM ref s3 = do
  modifyTVar ref $ \x ->
    x {ideStage3 = s3}
  pure ()

-- | Checks if the given ModuleName matches the last rebuild cache and if it
-- does returns all loaded definitions + the definitions inside the rebuild
-- cache
getAllModules2 :: Ide m => Maybe P.ModuleName -> m [Module]
getAllModules2 mmoduleName = do
  declarations <- s3Declarations <$> getStage3
  rebuild <- cachedRebuild
  case mmoduleName of
    Nothing -> pure (M.toList declarations)
    Just moduleName ->
      case rebuild of
        Just (cachedModulename, ef)
          | cachedModulename == moduleName -> do
              (AstData asts) <- s2AstData <$> getStage2
              let ast = fromMaybe M.empty (M.lookup moduleName asts)
              pure . M.toList $
                M.insert moduleName
                (snd . convertModule ast . convertExterns $ ef) declarations
        _ -> pure (M.toList declarations)

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
    x { ideStage3 = (ideStage3 x) {
          s3CachedRebuild = Just (efModuleName ef, ef)}}

-- | Retrieves the rebuild cache
cachedRebuild :: Ide m => m (Maybe (P.ModuleName, ExternsFile))
cachedRebuild = s3CachedRebuild <$> getStage3

-- | Extracts source spans from the parsed ASTs
populateStage2 :: (Ide m, MonadLogger m) => m ()
populateStage2 = do
  st <- ideStateVar <$> ask
  duration <- liftIO $ do
    start <- getTime Monotonic
    atomically (populateStage2STM st)
    end <- getTime Monotonic
    pure (Prelude.show (diffTimeSpec start end))
  $(logDebug) $ "Finished populating Stage2 in " <> toS duration

-- | STM version of populateStage2
populateStage2STM :: TVar IdeState -> STM ()
populateStage2STM ref = do
  modules <- s1Modules <$> getStage1STM ref
  let spans = map (\((P.Module ss _ _ decls _), _) -> M.fromList (concatMap (extractSpans ss) decls)) modules
  setStage2STM ref (Stage2 (AstData spans))

-- | Resolves reexports and populates Stage3 with data to be used in queries.
populateStage3 :: (Ide m, MonadLogger m) => m ()
populateStage3 = do
  st <- ideStateVar <$> ask
  duration <- liftIO $ do
    start <- getTime Monotonic
    atomically (populateStage3STM st)
    end <- getTime Monotonic
    pure (Prelude.show (diffTimeSpec start end))
  $(logDebug) $ "Finished populating Stage3 in " <> toS duration

-- | STM version of populateStage3
populateStage3STM :: TVar IdeState -> STM ()
populateStage3STM ref = do
  externs <- s1Externs <$> getStage1STM ref
  asts <- s2AstData <$> getStage2STM ref
  -- Build the "old" ExternDecl format
  let modules = M.mapKeys runModuleNameT (M.map (snd . convertExterns) externs)
      -- Convert ExternDecl into IdeDeclaration
      declarations = resolveReexports2 modules asts <$> M.toList modules
  setStage3STM ref (Stage3 (M.fromList declarations) Nothing)

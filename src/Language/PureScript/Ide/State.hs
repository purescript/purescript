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
  , resetIdeState
  , cacheRebuild
  , insertExterns
  , insertModule
  , insertExternsSTM
  , getAllModules
  , populateStage2
  , populateStage3
  , populateStage3STM
  -- for tests
  , resolveOperatorsForModule
  ) where

import           Protolude
import qualified Prelude

import           Control.Concurrent.STM
import           "monad-logger" Control.Monad.Logger
import qualified Data.Map.Lazy                     as Map
import qualified Data.List                         as List
import           Language.PureScript.Externs
import           Language.PureScript.Ide.Externs
import           Language.PureScript.Ide.Reexports
import           Language.PureScript.Ide.SourceFile
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import qualified Language.PureScript as P
import           System.Clock

-- | Resets all State inside psc-ide
resetIdeState :: Ide m => m ()
resetIdeState = do
  ideVar <- ideStateVar <$> ask
  liftIO . atomically $ do
    writeTVar ideVar emptyIdeState
    setStage3STM ideVar emptyStage3

-- | Gets the loaded Modulenames
getLoadedModulenames :: Ide m => m [P.ModuleName]
getLoadedModulenames = Map.keys <$> getExternFiles

-- | Gets all loaded ExternFiles
getExternFiles :: Ide m => m (Map P.ModuleName ExternsFile)
getExternFiles = s1Externs <$> getStage1

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
          s1Modules = Map.insert
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
setStage3STM :: TVar IdeState -> Stage3 -> STM ()
setStage3STM ref s3 = do
  modifyTVar ref $ \x ->
    x {ideStage3 = s3}
  pure ()

-- | Checks if the given ModuleName matches the last rebuild cache and if it
-- does returns all loaded definitions + the definitions inside the rebuild
-- cache
getAllModules :: Ide m => Maybe P.ModuleName -> m [Module]
getAllModules mmoduleName = do
  declarations <- s3Declarations <$> getStage3
  rebuild <- cachedRebuild
  case mmoduleName of
    Nothing -> pure (Map.toList declarations)
    Just moduleName ->
      case rebuild of
        Just (cachedModulename, ef)
          | cachedModulename == moduleName -> do
              (AstData asts) <- s2AstData <$> getStage2
              let
                ast =
                  fromMaybe (Map.empty, Map.empty) (Map.lookup moduleName asts)
                cachedModule =
                  snd . annotateModule ast . fst . convertExterns $ ef
                tmp =
                  Map.insert moduleName cachedModule declarations
                resolved =
                  Map.adjust (resolveOperatorsForModule tmp) moduleName tmp

              pure (Map.toList resolved)
        _ -> pure (Map.toList declarations)

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
          s1Externs = Map.insert (efModuleName ef) ef (s1Externs (ideStage1 x))}}

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
  let astData = map (extractAstInformation . fst) modules
  setStage2STM ref (Stage2 (AstData astData))

-- | Resolves reexports and populates Stage3 with data to be used in queries.
populateStage3 :: (Ide m, MonadLogger m) => m ()
populateStage3 = do
  st <- ideStateVar <$> ask
  (duration, results) <- liftIO $ do
    start <- getTime Monotonic
    results <- atomically (populateStage3STM st)
    end <- getTime Monotonic
    pure (Prelude.show (diffTimeSpec start end), results)
  traverse_
    (logWarnN . prettyPrintReexportResult (runModuleNameT . fst))
    (filter reexportHasFailures results)
  $(logDebug) $ "Finished populating Stage3 in " <> toS duration

-- | STM version of populateStage3
populateStage3STM :: TVar IdeState -> STM [ReexportResult Module]
populateStage3STM ref = do
  externs <- s1Externs <$> getStage1STM ref
  (AstData asts) <- s2AstData <$> getStage2STM ref
  let modules = Map.map convertExterns externs
      nModules :: Map P.ModuleName (Module, [(P.ModuleName, P.DeclarationRef)])
      nModules = Map.mapWithKey
        (\moduleName (m, refs) ->
           (fromMaybe m $ annotateModule <$> Map.lookup moduleName asts <*> pure m, refs)) modules
      -- resolves reexports and discards load failures for now
      result = resolveReexports (map (snd . fst) nModules) <$> Map.elems nModules
      resultP = resolveOperators (Map.fromList (reResolved <$> result))
  setStage3STM ref (Stage3 resultP Nothing)
  pure result

resolveOperators
  :: Map P.ModuleName [IdeDeclarationAnn]
  -> Map P.ModuleName [IdeDeclarationAnn]
resolveOperators modules =
  map (resolveOperatorsForModule modules) modules

-- | Looks up the types and kinds for operators and assigns them to their
-- declarations
resolveOperatorsForModule
  :: Map P.ModuleName [IdeDeclarationAnn]
  -> [IdeDeclarationAnn]
  -> [IdeDeclarationAnn]
resolveOperatorsForModule modules = map (mapIdeDeclaration resolveOperator)
  where
    resolveOperator (IdeValueOperator
                    opName
                    i@(P.Qualified (Just moduleName)
                        (Left ident)) precedence assoc _) =
      let t = do
            sourceModule <- Map.lookup moduleName modules
            IdeValue _ tP <-
              List.find (\case
                            IdeValue iP _ -> iP == ident
                            _ -> False) (discardAnn <$> sourceModule)
            pure tP

      in IdeValueOperator opName i precedence assoc t
    resolveOperator (IdeValueOperator
                    opName
                    i@(P.Qualified (Just moduleName)
                        (Right ctor)) precedence assoc _) =
      let t = do
            sourceModule <- Map.lookup moduleName modules
            IdeDataConstructor _ _ tP <-
              List.find (\case
                            IdeDataConstructor cname _ _ -> ctor == cname
                            _ -> False) (discardAnn <$> sourceModule)
            pure tP

      in IdeValueOperator opName i precedence assoc t
    resolveOperator (IdeTypeOperator
                    opName
                    i@(P.Qualified (Just moduleName) properName) precedence assoc _)  =
      let k = do
            sourceModule <- Map.lookup moduleName modules
            IdeType _ kP <-
              List.find (\case
                            IdeType name _ -> name == properName
                            _ -> False) (discardAnn <$> sourceModule)
            pure kP

      in IdeTypeOperator opName i precedence assoc k
    resolveOperator x = x

mapIdeDeclaration :: (IdeDeclaration -> IdeDeclaration) -> IdeDeclarationAnn -> IdeDeclarationAnn
mapIdeDeclaration f (IdeDeclarationAnn ann decl) = IdeDeclarationAnn ann (f decl)

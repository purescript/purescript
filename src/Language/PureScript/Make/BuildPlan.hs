module Language.PureScript.Make.BuildPlan
  ( BuildPlan(bpEnv, bpIndex)
  , BuildJobResult(..)
  , Options(..)
  , isUpToDate
  , construct
  , getResult
  , getPrevResult
  , collectResults
  , markComplete
  , needsRebuild
  ) where

import Prelude

import Control.Concurrent.Async.Lifted qualified as A
import Control.Concurrent.Lifted qualified as C
import Control.Monad.Base (liftBase)
import Control.Monad (foldM, guard)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Foldable (foldl')
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isNothing, catMaybes)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Language.PureScript.AST (Module, getModuleName)
import Language.PureScript.Crash (internalError)
import Language.PureScript.CST qualified as CST
import Language.PureScript.Errors (MultipleErrors(..))
import Language.PureScript.Externs (ExternsFile)
import Language.PureScript.Make.Actions as Actions
import Language.PureScript.Make.Cache (CacheDb, CacheInfo, checkChanged)
import Language.PureScript.Make.ExternsDiff (ExternsDiff, emptyDiff)
import Language.PureScript.Names (ModuleName)
import Language.PureScript.Sugar.Names.Env (Env, primEnv)
import System.Directory (getCurrentDirectory)

-- This status tells if a module's exiting build artifacts are up to date with a
-- current module's content. It would be safe to re-use them, but only if
-- changes in its dependencies do require the module's rebuild.
newtype UpToDateStatus = UpToDateStatus Bool

isUpToDate :: UpToDateStatus -> Bool
isUpToDate (UpToDateStatus b) = b

data Prebuilt = Prebuilt
  { pbExternsFile :: ExternsFile
  }

-- | The BuildPlan tracks information about our build progress, and holds all
-- prebuilt modules for incremental builds.
data BuildPlan = BuildPlan
  { bpPrebuilt :: M.Map ModuleName Prebuilt
  -- ^ Valid prebuilt results for modules, that are needed for rebuild, but
  -- their rebuild is not required.
  , bpPreviousBuilt :: M.Map ModuleName (UpToDateStatus, Prebuilt)
  -- ^ Previously built results for modules that are potentially required to be
  -- rebuilt. We will always rebuild not up to date modules. But we will only
  -- rebuild up to date modules, if their deps' externs have effectively
  -- changed. Previously built result is needed to compare previous and newly
  -- built externs to know what have changed.
  , bpBuildJobs :: M.Map ModuleName BuildJob
  , bpEnv :: C.MVar Env
  , bpIndex :: C.MVar Int
  }


newtype BuildJob = BuildJob
  { bjResult :: C.MVar BuildJobResult
    -- ^ Note: an empty MVar indicates that the build job has not yet finished.
  }

data BuildJobResult
  = BuildJobSucceeded !MultipleErrors !ExternsFile (Maybe ExternsDiff)
  -- ^ Succeeded, with warnings and externs, also holds externs diff with
  -- previous build result if any (lazily evaluated).
  --
  | BuildJobFailed !MultipleErrors
  -- ^ Failed, with errors.

  | BuildJobSkipped
  -- ^ The build job was not run, because an upstream build job failed.

type SuccessResult = (MultipleErrors, (ExternsFile, Maybe ExternsDiff))

buildJobSuccess :: BuildJobResult -> Maybe SuccessResult
buildJobSuccess (BuildJobSucceeded warnings externs diff) = Just (warnings, (externs, diff))
buildJobSuccess _ = Nothing

-- | Information obtained about a particular module while constructing a build
-- plan; used to decide whether a module needs rebuilding.
data RebuildStatus = RebuildStatus
  { rsModuleName :: ModuleName
  , rsRebuildNever :: Bool
  , rsNewCacheInfo :: Maybe CacheInfo
    -- ^ New cache info for this module which should be stored for subsequent
    -- incremental builds. A value of Nothing indicates that cache info for
    -- this module should not be stored in the build cache, because it is being
    -- rebuilt according to a RebuildPolicy instead.
  , rsPrebuilt :: Maybe UTCTime
    -- ^ Prebuilt timestamp (compilation time) for this module.
  , rsUpToDate :: Bool
    -- ^ Whether or not module (timestamp or content) changed since previous
    -- compilation (checked against provided cache-db info).
  }

-- | Construct common error message indicating a bug in the internal logic
barrierError :: T.Text -> a
barrierError infx = internalError $ "make: " <> T.unpack infx <> " no barrier"

-- | Called when we finished compiling a module and want to report back the
-- compilation result, as well as any potential errors that were thrown.
markComplete
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> ModuleName
  -> BuildJobResult
  -> m ()
markComplete buildPlan moduleName result = do
  let BuildJob rVar =
        fromMaybe (barrierError "markComplete") $ M.lookup moduleName (bpBuildJobs buildPlan)
  C.putMVar rVar result

-- | Whether or not the module with the given ModuleName needs to be rebuilt
needsRebuild :: BuildPlan -> ModuleName -> Bool
needsRebuild bp moduleName = M.member moduleName (bpBuildJobs bp)

-- | Collects results for all prebuilt as well as rebuilt modules. This will
-- block until all build jobs are finished. Prebuilt modules always return no
-- warnings.
collectResults
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> m (M.Map ModuleName BuildJobResult)
collectResults buildPlan = do
  let mapExts exts = BuildJobSucceeded (MultipleErrors []) exts Nothing
  let prebuiltResults =
        M.map (mapExts . pbExternsFile) (bpPrebuilt buildPlan)
  barrierResults <- traverse (C.readMVar . bjResult) $ bpBuildJobs buildPlan
  pure (M.union prebuiltResults barrierResults)

-- | Gets the the build result for a given module name independent of whether it
-- was rebuilt or prebuilt. Prebuilt modules always return no warnings.
getResult
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> ModuleName
  -> m (Maybe SuccessResult)
getResult buildPlan moduleName =
  case M.lookup moduleName (bpBuildJobs buildPlan) of
    Just bj ->
      buildJobSuccess <$> C.readMVar (bjResult bj)
    Nothing -> do
      let exts = pbExternsFile
            $ fromMaybe (barrierError "getResult")
            $ M.lookup moduleName (bpPrebuilt buildPlan)
      pure (Just (MultipleErrors [], (exts, Just $ emptyDiff moduleName )))

-- | Gets preloaded previous built result for modules that are going to be built. This
-- will be used to skip compilation if dep's externs have not changed.
getPrevResult :: BuildPlan -> ModuleName -> Maybe (UpToDateStatus, ExternsFile)
getPrevResult buildPlan moduleName =
  fmap pbExternsFile <$> M.lookup moduleName (bpPreviousBuilt buildPlan)

data Options = Options
  { optPreloadAllExterns :: Bool
  }

-- | Constructs a BuildPlan for the given module graph.
--
-- The given MakeActions are used to collect various timestamps in order to
-- determine whether a module needs rebuilding.
construct
  :: forall m. MonadBaseControl IO m
  => Options
  -> MakeActions m
  -> CacheDb
  -> ([CST.PartialResult Module], [(ModuleName, [ModuleName])])
  -> m (BuildPlan, CacheDb)
construct Options{..} MakeActions{..} cacheDb (sorted, graph) = do
  let sortedModuleNames = map (getModuleName . CST.resPartial) sorted
  rebuildStatuses <- A.forConcurrently sortedModuleNames getRebuildStatus

  -- Split modules into those that have to be rebuilt and those that have a valid
  -- prebuilt input. The Bool value in rebuildMap means if we may skip the
  -- compilation (if externs of dependencies have not changed). If it is False we
  -- should re-compile the module due to the following: the module's source have
  -- changed or some of dependencies were compiled later than the module.
  let (rebuildMap, prebuiltMap) = splitModules rebuildStatuses

  let toBeRebuilt = M.keys rebuildMap

  -- Set of all dependencies of modules to be rebuilt.
  let allBuildDeps = S.unions (S.fromList . moduleDeps <$> toBeRebuilt)
  let inBuildDeps = flip S.member allBuildDeps

  -- We only need prebuilt results for deps of the modules to be build.
  let toLoadPrebuilt =
        if optPreloadAllExterns
          then prebuiltMap
          else M.filterWithKey (const . inBuildDeps) prebuiltMap

  -- We will need previously built results for modules to be built
  -- to skip rebuilding if deps have not changed.
  let toLoadPrev =
        M.mapMaybeWithKey
          ( \mn prev -> do
              -- We load previous build result for all up-to-date modules, and
              -- also for changed modules that have dependants.
              status <- fst <$> prev
              guard (isUpToDate status || inBuildDeps mn)
              prev
          )
          rebuildMap

  (prebuiltLoad, prevLoad) <-
    A.concurrently
      (A.mapConcurrently id $ M.mapWithKey loadPrebuilt toLoadPrebuilt)
      (A.mapConcurrently id $ M.mapWithKey
        (\mn (up, ts) -> fmap (up,) <$> loadPrebuilt mn ts) toLoadPrev)

  let prebuilt = M.mapMaybe id prebuiltLoad
  let previous = M.mapMaybe id prevLoad

  -- If for some reason (wrong version, files corruption, etc) prebuilt
  -- externs loading fails, those modules should be rebuilt too.
  let failedLoads = M.keys $ M.filter isNothing prebuiltLoad
  buildJobs <- foldM makeBuildJob M.empty (toBeRebuilt <> failedLoads)

  env <- C.newMVar primEnv
  idx <- C.newMVar 1
  pure
    ( BuildPlan prebuilt previous buildJobs env idx
    , let
        update = flip $ \s ->
          M.alter (const (rsNewCacheInfo s)) (rsModuleName s)
      in
        foldl' update cacheDb rebuildStatuses
    )
  where
    -- Timestamp here is just to ensure that we will only try to load modules
    -- that have previous built results available.
    loadPrebuilt :: ModuleName -> UTCTime -> m (Maybe Prebuilt)
    loadPrebuilt = const . fmap (fmap Prebuilt . snd) . readExterns

    makeBuildJob prev moduleName = do
      buildJob <- BuildJob <$> C.newEmptyMVar
      pure (M.insert moduleName buildJob prev)

    getRebuildStatus :: ModuleName -> m RebuildStatus
    getRebuildStatus moduleName = do
      inputInfo <- getInputTimestampsAndHashes moduleName
      case inputInfo of
        Left RebuildNever -> do
          timestamp <- getOutputTimestamp moduleName
          pure (RebuildStatus
            { rsModuleName = moduleName
            , rsRebuildNever = True
            , rsPrebuilt = timestamp
            , rsUpToDate = True
            , rsNewCacheInfo = Nothing
            })
        Left RebuildAlways -> do
          pure (RebuildStatus
            { rsModuleName = moduleName
            , rsRebuildNever = False
            , rsPrebuilt = Nothing
            , rsUpToDate = False
            , rsNewCacheInfo = Nothing
            })
        Right cacheInfo -> do
          cwd <- liftBase getCurrentDirectory
          (newCacheInfo, upToDate) <- checkChanged cacheDb moduleName cwd cacheInfo
          timestamp <- getOutputTimestamp moduleName
          pure (RebuildStatus
            { rsModuleName = moduleName
            , rsRebuildNever = False
            , rsPrebuilt = timestamp
            , rsUpToDate = upToDate
            , rsNewCacheInfo = Just newCacheInfo
            })

    moduleDeps = fromMaybe graphError . flip lookup graph
      where
        graphError = internalError "make: module not found in dependency graph."

    splitModules :: [RebuildStatus] -> (M.Map ModuleName (Maybe (UpToDateStatus, UTCTime)), M.Map ModuleName UTCTime)
    splitModules = foldl' collectByStatus (M.empty, M.empty)

    collectByStatus (build, prebuilt) (RebuildStatus mn rebuildNever _ mbPb upToDate)
      -- To build if no prebuilt result exits.
      | Nothing <- mbPb = (M.insert mn Nothing build, prebuilt)
      -- To build if not up to date.
      | Just pb <- mbPb, not upToDate = toRebuild (False, pb)
      -- To prebuilt because of policy.
      | Just pb <- mbPb, rebuildNever = toPrebuilt pb
      -- In other case analyze compilation times of dependencies.
      | Just pb <- mbPb = do
          let deps = moduleDeps mn
          let modTimes = map (flip M.lookup prebuilt) deps

          case maximumMaybe (catMaybes modTimes) of
                -- Check if any of deps where build later. This means we should
                -- recompile even if the module's source is up-to-date. This may
                -- happen due to some partial builds or ide compilation
                -- workflows involved that do not assume full project
                -- compilation. We should treat those modules as NOT up to date
                -- to ensure they are rebuilt.
                Just depModTime | pb < depModTime -> toRebuild (False, pb)
                -- If one of the deps is not in the prebuilt, though the module
                -- is up to date, we should add it in the rebuild queue.
                _ | any isNothing modTimes -> toRebuild (upToDate, pb)
                _ -> toPrebuilt pb
        where
          toRebuild (up, t) = (M.insert mn (Just (UpToDateStatus up, t)) build, prebuilt)
          toPrebuilt v = (build, M.insert mn v prebuilt)

maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just $ maximum xs

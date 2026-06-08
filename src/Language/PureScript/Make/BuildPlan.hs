module Language.PureScript.Make.BuildPlan
  ( BuildPlan(bpEnv, bpIndex)
  , BuildJobResult(..)
  , Options(..)
  , getBuildReason
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
import Control.Monad (foldM, guard)
import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Data.Foldable (foldl')
import Data.Function (on)
import Data.List (maximumBy)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Language.PureScript.AST (Module, getModuleName, getModuleSourceSpan, spanName)
import Language.PureScript.CST qualified as CST
import Language.PureScript.Crash (internalError)
import Language.PureScript.Errors (MultipleErrors (..))
import Language.PureScript.Externs (ExternsFile)
import Language.PureScript.Make.Actions (MakeActions (..), RebuildPolicy (..), RebuildReason (..))
import Language.PureScript.Make.Cache (CacheDb, CacheInfo, checkChanged)
import Language.PureScript.Make.ExternsDiff (ExternsDiff, checkDiffs, emptyDiff)
import Language.PureScript.ModuleDependencies (ModuleGraph', DependencyDepth (..))
import Language.PureScript.Names (ModuleName)
import Language.PureScript.Sugar.Names.Env (Env, primEnv)
import System.Directory (getCurrentDirectory)
import Control.Applicative ((<|>))

data Prebuilt = Prebuilt
  { pbExterns :: ExternsFile
  , pbWarnings :: MultipleErrors
  }

-- | The BuildPlan tracks information about our build progress, and holds all
-- prebuilt modules for incremental builds.
data BuildPlan = BuildPlan
  { bpPrebuilt :: M.Map ModuleName Prebuilt
  -- ^ Valid prebuilt results for modules (rebuild it not required), that are
  -- needed for rebuild.
  , bpPrevious :: M.Map ModuleName (Maybe RebuildReason, Prebuilt)
  -- ^ Modules with available previously compiled results that may need to be
  -- re-compiled (if upstream effectively changed). If rebuild reason is known
  --  in advance (CacheOutdated, LaterDependency), the module will be rebuilt
  -- anyway, otherwise will be checked against deps for effective changes.
  , bpNoPrevious :: M.Map ModuleName RebuildReason
  -- ^ Modules with no previously built results that have to be rebuilt.
  , bpBuildJobs :: M.Map ModuleName BuildJob
  , bpEnv :: C.MVar Env
  , bpIndex :: C.MVar Int
  }

newtype BuildJob = BuildJob
  { bjResult :: C.MVar BuildJobResult
    -- ^ Note: an empty MVar indicates that the build job has not yet finished.
  }

data BuildJobResult
  = BuildJobSucceeded (Maybe RebuildReason) !MultipleErrors !ExternsFile (Maybe ExternsDiff)
  -- ^ Succeeded, with warnings and externs, also holds (lazily evaluated)
  -- externs diff with previous build result if there was one.
  --
  | BuildJobFailed !MultipleErrors
  -- ^ Failed, with errors.
  --
  | BuildJobSkipped
  -- ^ The build job was not run, because an upstream build job failed.

buildJobSuccess :: BuildJobResult -> Maybe (ExternsFile, Maybe ExternsDiff)
buildJobSuccess (BuildJobSucceeded _ _ externs diff) = Just (externs, diff)
buildJobSuccess _ = Nothing

-- Just a wrapper to make it clear what this time is about.
newtype OutputTimestamp = OutputTimestamp UTCTime deriving (Eq, Ord, Show)

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
  , rsPrevious :: Maybe OutputTimestamp
    -- ^ Prebuilt timestamp (compilation time) for this module.
  , rsUpToDate :: Bool
    -- ^ Whether or not module (timestamp or content) changed since previous
    -- compilation (checked against provided cache-db info).
  } deriving Show

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
  -> Bool
  -> m (M.Map ModuleName BuildJobResult)
collectResults buildPlan withPrebuilt = do
  let mapExts pb = BuildJobSucceeded Nothing (pbWarnings pb) (pbExterns pb) Nothing
  let prebuiltResults =
        M.map mapExts (bpPrebuilt buildPlan)

  barrierResults <- traverse (C.readMVar . bjResult) $ bpBuildJobs buildPlan
  pure $ if withPrebuilt then M.union prebuiltResults barrierResults else barrierResults

-- | Gets the the build result for a given module name independent of whether it
-- was rebuilt or prebuilt. Prebuilt modules always return no warnings.
getResult
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> ModuleName
  -> m (Maybe (ExternsFile, Maybe ExternsDiff))
getResult buildPlan moduleName =
  case M.lookup moduleName (bpBuildJobs buildPlan) of
    Just bj ->
      buildJobSuccess <$> C.readMVar (bjResult bj)
    -- If not build job for modules means it has prebuilt results.
    Nothing -> do
      let exts = pbExterns
            $ fromMaybe (barrierError "getResult")
            $ M.lookup moduleName (bpPrebuilt buildPlan)
      pure (Just (exts, Just $ emptyDiff moduleName ))

-- | Gets preloaded previous built result for modules that are going to be built. This
-- will be used to skip compilation if dep's externs have not changed.
getPrevResult :: BuildPlan -> ModuleName -> Maybe (ExternsFile, MultipleErrors)
getPrevResult buildPlan moduleName =
  (,) <$> pbExterns <*> pbWarnings <$> snd <$> M.lookup moduleName (bpPrevious buildPlan)

-- Get the build reason.
getBuildReason :: BuildPlan -> Module -> Maybe [ExternsDiff] -> Either (ExternsFile, MultipleErrors) RebuildReason
getBuildReason (BuildPlan {..}) m depsDiffs
  -- This should be refactored.
  | Nothing <- depsDiffs = Right NoCachedDependency
  | Just (Just reason, _)  <- prevResult = Right reason
  | Just (Nothing, exts) <- prevResult =
      case checkDiffs m <$> depsDiffs of
        Just (Just diffRef) -> Right (UpstreamRef diffRef)
        (Just Nothing) -> Left (pbExterns exts, pbWarnings exts)
        Nothing -> Right NoCachedDependency
  | otherwise = Right $ fromMaybe (barrierError "getBuildReason") (M.lookup mn bpNoPrevious)

  where
  prevResult = M.lookup mn bpPrevious
  mn = getModuleName m

data Options = Options
  { optPreloadAllExterns :: Bool
  }

type RebuildMap = M.Map ModuleName (Maybe RebuildReason, Maybe OutputTimestamp)
type PrebuiltMap = M.Map ModuleName OutputTimestamp

-- | Constructs a BuildPlan for the given module graph.
--
-- The given MakeActions are used to collect various timestamps in order to
-- determine whether a module needs rebuilding.
construct
  :: forall m. MonadBaseControl IO m
  => Options
  -> MakeActions m
  -> CacheDb
  -> ([CST.PartialResult Module], ModuleGraph')
  -> m (BuildPlan, CacheDb)
construct Options{..} MakeActions{..} cacheDb (sorted, graph) = do
  let sortedModuleNames = map getMName sorted
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

  -- We only need prebuilt results for deps will be required during the build.
  let toLoadPrebuilt =
        if optPreloadAllExterns
          then prebuiltMap
          else M.filterWithKey (const . inBuildDeps) prebuiltMap

  -- We will need previously built results for modules to be built
  -- to skip rebuilding if deps have not changed.
  let toLoadPrev =
        M.mapMaybeWithKey
          ( \mn (mbRebuildReason, mbTs) -> do
              -- We load previous build result for all up-to-date modules, and
              -- also for changed modules that have dependants (to get diffs).
              -- We don't load for those who have no dependencies, because diffs
              -- will not be needed.
              ts <- mbTs
              guard (isNothing mbRebuildReason || inBuildDeps mn)
              pure (mbRebuildReason, ts)
          )
          rebuildMap

  -- Store known build reasons for modules that do not have actual prebuilt.
  let noPrebuilt = M.mapMaybe id $ fst <$> M.difference rebuildMap toLoadPrev

  (prebuiltLoad, prevLoad) <-
    A.concurrently
      (A.mapConcurrently id $ M.mapWithKey loadPrevious toLoadPrebuilt)
      (A.mapConcurrently id $ M.mapWithKey
        (\mn (up, ts) -> fmap (up,) <$> loadPrevious mn ts) toLoadPrev)

  let prebuilt = M.mapMaybe id prebuiltLoad
  let previous = M.mapMaybe id prevLoad

  -- If for some reason (wrong version, files corruption, etc) prebuilt
  -- externs loading fails, those modules should be rebuilt too.
  let failedLoads = M.keys $ M.filter isNothing prebuiltLoad
  buildJobs <- foldM makeBuildJob M.empty (toBeRebuilt <> failedLoads)

  env <- C.newMVar primEnv
  idx <- C.newMVar 1
  pure
    ( BuildPlan prebuilt previous noPrebuilt buildJobs env idx
    , let
        update = flip $ \s ->
          M.alter (const (rsNewCacheInfo s)) (rsModuleName s)
      in
        foldl' update cacheDb rebuildStatuses
    )
  where
    getMName = getModuleName . CST.resPartial
    getSpName = spanName . getModuleSourceSpan . CST.resPartial

    -- Module to FilePath map used for loading prebuilt warnings.
    fpMap = M.fromList $ (,) <$> getMName <*> getSpName <$> sorted

    -- Timestamp here is just to ensure that we will only try to load modules
    -- that have previous built results available.
    loadPrevious :: ModuleName -> OutputTimestamp -> m (Maybe Prebuilt)
    loadPrevious mn _ = do
      externs <- snd <$> readExterns mn
      case externs of
        Just exts ->  do
          warnings <- fromMaybe (MultipleErrors []) <$>
            case M.lookup mn fpMap of
              Just fp ->  snd <$> readWarnings (mn, fp)
              _ -> pure Nothing
          pure $ Just $ Prebuilt exts warnings
        _ ->
          pure Nothing


    makeBuildJob prev moduleName = do
      buildJob <- BuildJob <$> C.newEmptyMVar
      pure (M.insert moduleName buildJob prev)

    getRebuildStatus :: ModuleName -> m RebuildStatus
    getRebuildStatus moduleName = do
      inputInfo <- getInputTimestampsAndHashes moduleName
      case inputInfo of
        Left RebuildNever -> do
          timestamp <- fmap OutputTimestamp <$> getOutputTimestamp moduleName
          pure (RebuildStatus
            { rsModuleName = moduleName
            , rsRebuildNever = True
            -- rsRebuildReason: Nothing -- if not prebuilt?
            , rsPrevious = timestamp
            , rsNewCacheInfo = Nothing
            , rsUpToDate = True
            })
        Left RebuildAlways -> do
          pure (RebuildStatus
            { rsModuleName = moduleName
            , rsRebuildNever = False
            , rsPrevious = Nothing
            , rsNewCacheInfo = Nothing
            , rsUpToDate = False
            })
        Right cacheInfo -> do
          cwd <- liftBase getCurrentDirectory
          (newCacheInfo, upToDate) <- checkChanged cacheDb moduleName cwd cacheInfo
          timestamp <- fmap OutputTimestamp <$> getOutputTimestamp moduleName

          pure (RebuildStatus
            { rsModuleName = moduleName
            , rsRebuildNever = False
            , rsPrevious = timestamp
            , rsNewCacheInfo = Just newCacheInfo
            , rsUpToDate = upToDate
            })

    moduleDeps = map snd . fromMaybe graphError . flip lookup graph
      where
        graphError = internalError "make: module not found in dependency graph."

    moduleDirectDeps =  map snd . filter ((==) Direct . fst) .  fromMaybe graphError . flip lookup graph
      where
        graphError = internalError "make: module not found in dependency graph."

    splitModules :: [RebuildStatus] -> (RebuildMap, PrebuiltMap)
    splitModules = foldl' collectByStatus (M.empty, M.empty)

    collectByStatus (build, prebuilt) (RebuildStatus mn rebuildNever cacheInfo mbPb upToDate)
      -- If no cacheInfo => RebuildAlwaysPolicy.
      | Nothing <- mbPb, Nothing <- cacheInfo, not upToDate =
          (M.insert mn (Just RebuildAlwaysPolicy, Nothing) build, prebuilt)
      -- In other cases if no previous, even if RebuildNever policy.
      | Nothing <- mbPb =
          (M.insert mn (Just NoCached, Nothing) build, prebuilt)
      | Just pb <- mbPb, not upToDate = toRebuild (Just CacheOutdated, pb)
      -- Treat as prebuilt because of RebuildNever policy.
      | Just pb <- mbPb, rebuildNever = toPrebuilt pb
      -- In other case analyze compilation times of dependencies.
      | Just pb <- mbPb = do
          -- We may check only direct dependencies here because transitive
          -- changes (caused by reexports) will be propagated by externs diffs
          -- of direct dependencies.
          let deps = moduleDirectDeps mn

          let modTimes = map (\dmn -> (,) dmn <$> (M.lookup dmn prebuilt <|> (snd =<< M.lookup dmn build))) deps
          let modTimes' = map (\dmn -> (,) dmn <$> M.lookup dmn prebuilt) deps

          case maximumMaybe (catMaybes modTimes) of
                -- Check if any of deps where build later. This means we should
                -- recompile even if the module's source is up-to-date. This may
                -- happen due to some partial builds or ide compilation
                -- workflows involved that do not assume full project
                -- compilation.
                Just (dmn, depModTime) | pb < depModTime -> toRebuild (Just (LaterDependency dmn), pb)
                -- If one of the deps (even though it may have previous result
                -- available) is not in the prebuilt, we should add the module
                -- in the rebuild queue (where it will be checked against deps'
                -- changes).
                _ | any isNothing modTimes' -> toRebuild (Nothing, pb)
                _ -> toPrebuilt pb
        where
          toRebuild (mbReason, t) = (M.insert mn (mbReason, Just t) build, prebuilt)
          toPrebuilt v = (build, M.insert mn v prebuilt)

maximumMaybe :: Ord a => [(ModuleName, a)] -> Maybe (ModuleName, a)
maximumMaybe [] = Nothing
maximumMaybe xs = Just $ maximumBy (compare `on` snd) xs

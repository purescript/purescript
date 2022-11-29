{-# LANGUAGE TypeApplications #-}
module Language.PureScript.Make.BuildPlan
  ( BuildPlan(bpEnv, bpIndex)
  , BuildJobResult(..)
  , DidPublicApiChange(..)
  , buildJobSucceeded
  , buildJobSuccess
  , construct
  , getResult
  , collectResults
  , markComplete
  , needsRebuild
  --
  , bjResult
  , bpBuildJobs
  , pbExternsFile
  , bpPrebuilt
  , bjPrebuilt
  , bjDirtyExterns
  , shouldWeRebuild
  ) where

import           Prelude

import Codec.Serialise as Serialise
import           Control.Concurrent.Async.Lifted as A
import           Control.Concurrent.Lifted as C
import           Control.Monad.Base (liftBase)
import           Control.Monad hiding (sequence)
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.Foldable (foldl')
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Time.Clock (UTCTime(..))
import           Language.PureScript.AST
import           Language.PureScript.Crash
import qualified Language.PureScript.CST as CST
import           Language.PureScript.Errors
import           Language.PureScript.Externs
import           Language.PureScript.Make.Actions as Actions
import           Language.PureScript.Make.Cache
import           Language.PureScript.Names (ModuleName(..))
import           Language.PureScript.Sugar.Names.Env
import           System.Directory (getCurrentDirectory)
import Data.Function

-- for debug prints, timestamps
-- import Debug.Trace
-- import Language.PureScript.Docs.Types (formatTime)
-- import Data.Time.Clock (getCurrentTime)
-- import System.IO.Unsafe (unsafePerformIO)
-- {-# NOINLINE dt #-}
-- dt :: IO String
-- dt = do
--   ts <- getCurrentTime
--   pure (formatTime ts)


-- | The BuildPlan tracks information about our build progress, and holds all
-- prebuilt modules for incremental builds.
data BuildPlan = BuildPlan
  { bpPrebuilt :: M.Map ModuleName Prebuilt
  , bpBuildJobs :: M.Map ModuleName BuildJob
  , bpEnv :: C.MVar Env
  , bpIndex :: C.MVar Int
  }

data Prebuilt = Prebuilt
  { pbModificationTime :: UTCTime
  , pbExternsFile :: ExternsFile
  }

data BuildJob = BuildJob
  { bjResult :: C.MVar BuildJobResult
    -- ^ Note: an empty MVar indicates that the build job has not yet finished.
    -- TODO[drathier]: remove both fields here and newtype BuildJob again:
  , bjPrebuilt :: Maybe Prebuilt
  , bjDirtyExterns :: Maybe ExternsFile
  }

data BuildJobResult
  = BuildJobSucceeded !MultipleErrors !ExternsFile !DidPublicApiChange
  -- ^ Succeeded, with warnings and externs
  --
  | BuildJobCacheHit !ExternsFile
  -- ^ Cache hit, so no warnings
  --
  | BuildJobFailed !MultipleErrors
  -- ^ Failed, with errors

  | BuildJobSkipped
  -- ^ The build job was not run, because an upstream build job failed

data DidPublicApiChange
  = PublicApiChanged
  | PublicApiStayedTheSame
  deriving (Show, Eq)

buildJobSucceeded :: Maybe BuildCacheFile -> MultipleErrors -> ExternsFile -> BuildJobResult
buildJobSucceeded mDirtyCache warnings externs =
  case mDirtyCache of
    Just dirtyCache | fastEqBuildCache dirtyCache (efBuildCache externs) -> BuildJobSucceeded warnings externs PublicApiStayedTheSame
    _ -> BuildJobSucceeded warnings externs PublicApiChanged

fastEqBuildCache :: BuildCacheFile -> BuildCacheFile -> Bool
fastEqBuildCache cache externsCache =
  let
    toCmp (BuildCacheFile bcVersion bcModuleName bcCacheBlob bcCacheDecls bcDeclarations bcDeclShapes _bcCacheDeps) =
      -- don't compare imports; it will result in two layers being rebuilt instead of one
      BuildCacheFile bcVersion bcModuleName bcCacheBlob bcCacheDecls bcDeclarations bcDeclShapes mempty
  in
  Serialise.serialise (toCmp cache) == Serialise.serialise (toCmp externsCache)

buildJobSuccess :: BuildJobResult -> Maybe (MultipleErrors, ExternsFile, DidPublicApiChange)
buildJobSuccess (BuildJobSucceeded warnings externs wasRebuildNeeded) = Just (warnings, externs, wasRebuildNeeded)
buildJobSuccess (BuildJobCacheHit externs) = Just (MultipleErrors [], externs, PublicApiStayedTheSame)
buildJobSuccess _ = Nothing


shouldWeRebuild
  :: MonadBaseControl IO m
  => ModuleName
  -> M.Map ModuleName (MVar BuildJobResult)
  -> M.Map ModuleName ()
  -> m Bool
shouldWeRebuild _moduleName deps directDeps = do
  let depChangedAndWeShouldBuild =
        \case
          BuildJobSucceeded _ _ PublicApiChanged -> True
          BuildJobSucceeded _ _ PublicApiStayedTheSame -> False
          BuildJobCacheHit _ -> False
          BuildJobSkipped -> False
          BuildJobFailed _ -> False

  let _bjKey =
        \case
          BuildJobSucceeded _ _ PublicApiChanged -> "BuildJobSucceeded:PublicApiChanged" :: String
          BuildJobSucceeded _ _ PublicApiStayedTheSame -> "BuildJobSucceeded:PublicApiStayedTheSame" :: String
          BuildJobCacheHit _ -> "BuildJobCacheHit" :: String
          BuildJobFailed _ -> "BuildJobFailed" :: String
          BuildJobSkipped -> "BuildJobSkipped" :: String

  -- did any dependency change?
  anyUpstreamChanges <-
    deps
      -- & (\v -> trace (show ("depsExternDecls1" :: String, M.keys v)) v)
      & (id :: M.Map ModuleName (MVar BuildJobResult) -> M.Map ModuleName (MVar BuildJobResult))
      & (\keepValues -> M.intersection keepValues directDeps)
      -- & (\v -> trace (show ("depsExternDecls2" :: String, M.keys v, "dirtyImportedModules" :: String, dirtyImportedModules)) v)
      & (id :: M.Map ModuleName (MVar BuildJobResult) -> M.Map ModuleName (MVar BuildJobResult))
      & traverse tryReadMVar
      & fmap (\bjmap ->
        bjmap
        -- & M.elems
        & fmap (fromMaybe (internalError "shouldWeRebuild1: no barrier"))
        & M.filter depChangedAndWeShouldBuild
        & \case
          m | M.null m -> False
          _changedDeps ->
            -- trace (show ("shouldWeRebuild:yes" :: String, moduleName, "changed" :: String, fmap bjKey changedDeps ))
            True
          -- v ->
          --   trace (show ("shouldWeRebuild:no" :: String,
          --       case v of
          --         BuildJobSucceeded _ _ PublicApiChanged -> "BuildJobSucceeded:PublicApiChanged" :: String
          --         BuildJobSucceeded _ _ PublicApiStayedTheSame -> "BuildJobSucceeded:PublicApiStayedTheSame" :: String
          --         BuildJobCacheHit _ -> "BuildJobCacheHit" :: String
          --         BuildJobFailed _ -> "BuildJobFailed" :: String
          --         BuildJobSkipped -> "BuildJobSkipped" :: String
          --     , M.keys directDeps))
          --    False
      )
  pure anyUpstreamChanges
  -- TODO[drathier]: we can do more here; if a dep changed but we don't import the changed thing, we can consider the dep unchanged

-- | Information obtained about a particular module while constructing a build
-- plan; used to decide whether a module needs rebuilding.
data RebuildStatus = RebuildStatus
  { statusModuleName :: ModuleName
  , statusRebuildNever :: Bool
  , statusNewCacheInfo :: Maybe CacheInfo
    -- ^ New cache info for this module which should be stored for subsequent
    -- incremental builds. A value of Nothing indicates that cache info for
    -- this module should not be stored in the build cache, because it is being
    -- rebuilt according to a RebuildPolicy instead.
  , statusPrebuilt :: Maybe Prebuilt
    -- ^ Prebuilt externs and timestamp for this module, if any.
  , statusDirtyExterns :: Maybe ExternsFile
    -- ^ Prebuilt externs and timestamp for this module, if any, but also present even if the source file is changed.
  }

-- | Called when we finished compiling a module and want to report back the
-- compilation result, as well as any potential errors that were thrown.
markComplete
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> ModuleName
  -> BuildJobResult
  -> m ()
markComplete buildPlan moduleName result = do
  let BuildJob rVar _ _ = fromMaybe (internalError "make: markComplete no barrier") $ M.lookup moduleName (bpBuildJobs buildPlan)
  putMVar rVar result

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
  let prebuiltResults = M.map (buildJobSucceeded Nothing (MultipleErrors []) . pbExternsFile) (bpPrebuilt buildPlan)
  barrierResults <- traverse (readMVar . bjResult) $ bpBuildJobs buildPlan
  pure (M.union prebuiltResults barrierResults)

-- | Gets the the build result for a given module name independent of whether it
-- was rebuilt or prebuilt. Prebuilt modules always return no warnings.
getResult
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> ModuleName
  -> m (Maybe (MultipleErrors, ExternsFile, DidPublicApiChange))
getResult buildPlan moduleName =
  case M.lookup moduleName (bpPrebuilt buildPlan) of
    Just es ->
      pure (Just (MultipleErrors [], pbExternsFile es, PublicApiStayedTheSame))
    Nothing -> do
      let bj = fromMaybe (internalError "make: no barrier") $ M.lookup moduleName (bpBuildJobs buildPlan)
      r <- readMVar $ bjResult bj
      pure $ buildJobSuccess r

-- | Constructs a BuildPlan for the given module graph.
--
-- The given MakeActions are used to collect various timestamps in order to
-- determine whether a module needs rebuilding.
construct
  :: forall m. (Monad m, MonadBaseControl IO m)
  => MakeActions m
  -> CacheDb
  -> ([CST.PartialResult Module], [(ModuleName, [ModuleName])])
  -> m (BuildPlan, CacheDb)
construct MakeActions{..} cacheDb (sorted, graph) = do
  let sortedModuleNames = map (getModuleName . CST.resPartial) sorted
  rebuildStatuses <- A.forConcurrently sortedModuleNames getRebuildStatus
  let prebuilt =
        foldl' collectPrebuiltModules M.empty $
          mapMaybe (\s -> (statusModuleName s, statusRebuildNever s,) <$> statusPrebuilt s) (snd <$> rebuildStatuses)
  let toBeRebuilt = filter (not . flip M.member prebuilt . fst) rebuildStatuses
  -- _ <- trace (show ("BuildPlan.construct 4 start" :: String, unsafePerformIO dt)) $ pure ()
  buildJobs <- foldM makeBuildJob M.empty toBeRebuilt
  -- _ <- trace (show ("BuildPlan.construct 5 start" :: String, unsafePerformIO dt)) $ pure ()
  env <- C.newMVar primEnv
  idx <- C.newMVar 1
  -- _ <- trace (show ("BuildPlan.construct 6 start" :: String, unsafePerformIO dt)) $ pure ()
  let res =
        ( BuildPlan prebuilt buildJobs env idx
        , let
            update = flip $ \s ->
              M.alter (const (statusNewCacheInfo s)) (statusModuleName s)
          in
            foldl' update cacheDb (snd <$> rebuildStatuses)
        )
  -- trace (show ("BuildPlan.construct 7 end" :: String, unsafePerformIO dt)) $ pure ()
  pure res
  where
    makeBuildJob prev (moduleName, rebuildStatus) = do
      buildJobMvar <- C.newEmptyMVar
      let buildJob = BuildJob buildJobMvar (statusPrebuilt rebuildStatus) (statusDirtyExterns rebuildStatus)
      pure (M.insert moduleName buildJob prev)

    getRebuildStatus :: ModuleName -> m (ModuleName, RebuildStatus)
    -- TODO[drathier]: statusDirtyExterns seemingly contains no more info than Prebuilt does; are we filtering Prebuilt but not DirtyExterns somewhere? Why have both?
    getRebuildStatus moduleName = (moduleName,) <$> do
      inputInfo <- getInputTimestampsAndHashes moduleName
      case inputInfo of
        Left RebuildNever -> do
          dirtyExterns <- snd <$> readExterns moduleName
          prebuilt <- findExistingExtern dirtyExterns moduleName
          pure (RebuildStatus
            { statusModuleName = moduleName
            , statusRebuildNever = True
            , statusPrebuilt = prebuilt
            , statusDirtyExterns = dirtyExterns
            , statusNewCacheInfo = Nothing
            })
        Left RebuildAlways -> do
          pure (RebuildStatus
            { statusModuleName = moduleName
            , statusRebuildNever = False
            , statusPrebuilt = Nothing
            , statusDirtyExterns = Nothing
            , statusNewCacheInfo = Nothing
            })
        Right cacheInfo -> do
          cwd <- liftBase getCurrentDirectory
          (newCacheInfo, isUpToDate) <- checkChanged cacheDb moduleName cwd cacheInfo
          dirtyExterns <- snd <$> readExterns moduleName
          prebuilt <-
            -- NOTE[fh]: prebuilt is Nothing for source-modified files, and Just for non-source modified files
            if isUpToDate
              then findExistingExtern dirtyExterns moduleName
              else pure Nothing
          pure (RebuildStatus
            { statusModuleName = moduleName
            , statusRebuildNever = False
            , statusPrebuilt = prebuilt
            , statusDirtyExterns = dirtyExterns
            , statusNewCacheInfo = Just newCacheInfo
            })

    findExistingExtern :: Maybe ExternsFile -> ModuleName -> m (Maybe Prebuilt)
    findExistingExtern mexterns moduleName = runMaybeT $ do
      timestamp <- MaybeT $ getOutputTimestamp moduleName
      externs <- MaybeT $ pure mexterns
      pure (Prebuilt timestamp externs)

    collectPrebuiltModules :: M.Map ModuleName Prebuilt -> (ModuleName, Bool, Prebuilt) -> M.Map ModuleName Prebuilt
    collectPrebuiltModules prev (moduleName, rebuildNever, pb)
      | rebuildNever = M.insert moduleName pb prev
      | otherwise = do
          let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup moduleName graph)
          case traverse (fmap pbModificationTime . flip M.lookup prev) deps of
            Nothing ->
              -- If we end up here, one of the dependencies didn't exist in the
              -- prebuilt map and so we know a dependency might need to be rebuilt, which
              -- means we might need to be rebuilt in turn.
              prev
            Just modTimes ->
              -- TODO[drathier]: this feels too pessimistic, we might not have to rebuild even if a dep was modified; is this code intended to just filter out things we for sure won't have to rebuild, or to exactly say which files we should rebuild?
              case maximumMaybe modTimes of
                Just depModTime | pbModificationTime pb < depModTime ->
                  prev
                _ -> M.insert moduleName pb prev

maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just $ maximum xs

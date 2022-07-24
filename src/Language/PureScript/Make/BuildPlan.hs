{-# LANGUAGE TypeApplications #-}
module Language.PureScript.Make.BuildPlan
  -- ( BuildPlan(bpEnv)
  -- , BuildJobResult(..)
  -- , buildJobSuccess
  -- , construct
  -- , getResult
  -- , collectResults
  -- , markComplete
  -- , needsRebuild
  -- )
  where

import           Prelude

import Codec.Serialise as Serialise
import           Control.Concurrent.Async.Lifted as A
import           Control.Concurrent.Lifted as C
import           Control.Monad.Base (liftBase)
import           Control.Monad hiding (sequence)
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.Foldable (foldl')
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
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
import Data.Functor
import Debug.Trace
import qualified Data.ByteString.Lazy as B

-- for debug prints, timestamps
import Language.PureScript.Docs.Types (formatTime)
import Data.Time.Clock (getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE dt #-}
dt = do
  ts <- getCurrentTime
  pure (formatTime ts)


-- | The BuildPlan tracks information about our build progress, and holds all
-- prebuilt modules for incremental builds.
data BuildPlan = BuildPlan
  { bpPrebuilt :: M.Map ModuleName Prebuilt
  , bpBuildJobs :: M.Map ModuleName BuildJob
  , bpEnv :: C.MVar Env
  }

data Prebuilt = Prebuilt
  { pbModificationTime :: UTCTime
  , pbExternsFile :: ExternsFile
  }

data BuildJob = BuildJob
  { bjResult :: C.MVar BuildJobResult
    -- ^ Note: an empty MVar indicates that the build job has not yet finished.
  , bjPrebuilt :: Maybe Prebuilt
  , bjDirtyExterns :: Maybe ExternsFile
  }

data BuildJobResult
  = BuildJobSucceeded !MultipleErrors !ExternsFile !WasRebuildNeeded -- we built it, but was the rebuild actually needed?
  -- ^ Succeeded, with warnings and externs
  --
  | BuildJobCacheHit !ExternsFile
  -- ^ Cache hit, so no warnings
  --
  | BuildJobFailed !MultipleErrors
  -- ^ Failed, with errors

  | BuildJobSkipped
  -- ^ The build job was not run, because an upstream build job failed

data WasRebuildNeeded
  = RebuildWasNeeded
  | RebuildWasNotNeeded
  deriving (Show, Eq)

buildJobSucceeded :: Maybe ExternsFile -> MultipleErrors -> ExternsFile -> BuildJobResult
buildJobSucceeded mDirtyExterns warnings externs =
  case mDirtyExterns of
    Just dirtyExterns | fastEqExterns dirtyExterns externs -> BuildJobSucceeded warnings externs RebuildWasNotNeeded
    _ -> BuildJobSucceeded warnings externs RebuildWasNeeded

buildJobSuccess :: BuildJobResult -> Maybe (MultipleErrors, ExternsFile, WasRebuildNeeded)
-- buildJobSuccess (Just dirtyExterns) (BuildJobSucceeded warnings externs) | Serialise.serialise dirtyExterns == Serialise.serialise externs = Just (warnings, externs, RebuildWasNotNeeded)
buildJobSuccess (BuildJobSucceeded warnings externs wasRebuildNeeded) = Just (warnings, externs, wasRebuildNeeded)
buildJobSuccess (BuildJobCacheHit externs) = Just (MultipleErrors [], externs, RebuildWasNotNeeded)
buildJobSuccess _ = Nothing

fastEqExterns a b =
  let
    -- TODO[drathier]: is it enough to look at just the cacheDeclarations (what we export)? or do we need to look at the cached imports too?
    -- toCmp x = (bcCacheDeclarations $ efBuildCache x, (bcCacheImports . efBuildCache) x)
    toCmp x = bcCacheDeclarations $ efBuildCache x
  in
  Serialise.serialise (toCmp a) == Serialise.serialise (toCmp b)


isCacheHit
  :: MonadBaseControl IO m
  => M.Map ModuleName (MVar BuildJobResult)
  -> M.Map ModuleName ExternsFile
  -> ExternsFile
  -> m Bool
isCacheHit deps depsExternsFromPrebuilts dirtyExterns = do
  let
    -- was any of the direct deps RebuildWasNeeded? if so, rebuild.
    -- 1. find all direct deps by looking at the dirty externsfile
    dirtyExternsCachedImports :: M.Map ModuleName (M.Map B.ByteString [B.ByteString])
    dirtyExternsCachedImports =
      dirtyExterns
        & (bcCacheImports . efBuildCache)

  (depsExternDeclsFromMVars :: M.Map ModuleName ExternsFile) <-
      deps
      -- & (\v -> trace (show ("depsExternDecls1" :: String, M.keys v)) v)
      & (id :: M.Map ModuleName (MVar BuildJobResult) -> M.Map ModuleName (MVar BuildJobResult))
      & (\keepValues -> M.intersection keepValues dirtyExternsCachedImports)
      -- & (\v -> trace (show ("depsExternDecls2" :: String, M.keys v, "dirtyImportedModules" :: String, dirtyImportedModules)) v)
      & (id :: M.Map ModuleName (MVar BuildJobResult) -> M.Map ModuleName (MVar BuildJobResult))
      & traverse tryReadMVar
      -- -- & fmap (\v -> trace (show ("depsExternDecls3" :: String, v)) v)
      & (id :: m (M.Map ModuleName (Maybe BuildJobResult)) -> m (M.Map ModuleName (Maybe BuildJobResult)))
      & fmap (\v ->
        v
        & M.mapMaybe id
        -- & (\v -> trace (show ("depsExternDecls4" :: String, efModuleName dirtyExterns, M.keys v)) v)
        & (id :: M.Map ModuleName BuildJobResult -> M.Map ModuleName BuildJobResult)
        & traverse (\case
          BuildJobSucceeded _ externs RebuildWasNotNeeded ->
            -- trace (show ("isCacheHit" :: String, efModuleName dirtyExterns, "dep", "RebuiltWasNotNeeded", efModuleName externs)) $
            Just externs
          BuildJobSucceeded _ externs RebuildWasNeeded ->
            -- trace (show ("isCacheHit:no" :: String, efModuleName dirtyExterns, "dep", "RebuiltWasNeeded", efModuleName externs)) $
            Nothing
          BuildJobCacheHit externs ->
            -- trace (show ("isCacheHit" :: String, efModuleName dirtyExterns, "dep", "BuildJobCacheHit", efModuleName externs)) $
              Just externs
          BuildJobFailed _ -> Nothing
          BuildJobSkipped -> Nothing
        )
        -- & fromMaybe (internalError "isCacheHit: no barrier")
        & fromMaybe (trace (show ("isCacheHit:Nothing" :: String, efModuleName dirtyExterns)) mempty)
      )

  let (depsExternDecls :: M.Map ModuleName (M.Map B.ByteString [B.ByteString])) =
        (depsExternDeclsFromMVars <> depsExternsFromPrebuilts)
        & (\keepValues -> M.intersection keepValues dirtyExternsCachedImports)
        & (id :: M.Map ModuleName ExternsFile -> M.Map ModuleName ExternsFile)
        & fmap (bcCacheDeclarations . efBuildCache)
        -- TODO[drathier]: only look at the keys we care about
        -- & (\v -> trace (show ("depsExternDecls6" :: String, M.keys v)) v)
        & (id :: M.Map ModuleName (M.Map B.ByteString [B.ByteString]) -> M.Map ModuleName (M.Map B.ByteString [B.ByteString]))

  pure $
    case depsExternDecls == dirtyExternsCachedImports of
      False ->
        -- trace (show ("isCacheHit1 cache miss" :: String, efModuleName dirtyExterns, ("deps-len", length deps), ("dirtyCachedImports" :: String, M.keys dirtyExternsCachedImports))) $
        False
      True ->
        -- trace (show ("isCacheHit1 cache hit" :: String, efModuleName dirtyExterns, ("deps-len", length deps), ("dirtyCachedImports" :: String, M.keys dirtyExternsCachedImports))) $
        True


isCacheHit deps depsExternsFromPrebuilts dirtyExterns = do
  let


    externFromBJRes = \case
      BuildJobSucceeded _ e _ -> Just e -- TODO[drathier]: look at the rebuild flag!
      BuildJobCacheHit e -> Just e
      BuildJobFailed _ -> Nothing
      BuildJobSkipped -> Nothing

    dirtyCachedImports :: M.Map ModuleName (M.Map B.ByteString [B.ByteString])
    dirtyCachedImports =
      dirtyExterns
        & (bcCacheImports . efBuildCache)

    dirtyImportedModules :: [ModuleName]
    dirtyImportedModules =
      dirtyExterns
        & efImports
        <&> eiModule
        & L.nub

  (depsExternDeclsFromMVars :: M.Map ModuleName ExternsFile) <-
    deps
    -- & (\v -> trace (show ("depsExternDecls1" :: String, M.keys v)) v)
    & (id :: M.Map ModuleName (MVar BuildJobResult) -> M.Map ModuleName (MVar BuildJobResult))
    & M.filterWithKey (\k _ -> elem k dirtyImportedModules)
    -- & (\v -> trace (show ("depsExternDecls2" :: String, M.keys v, "dirtyImportedModules" :: String, dirtyImportedModules)) v)
    & (id :: M.Map ModuleName (MVar BuildJobResult) -> M.Map ModuleName (MVar BuildJobResult))
    & traverse tryReadMVar
    -- -- & fmap (\v -> trace (show ("depsExternDecls3" :: String, v)) v)
    & (id :: m (M.Map ModuleName (Maybe BuildJobResult)) -> m (M.Map ModuleName (Maybe BuildJobResult)))
    & fmap (\v ->
      v
      & M.mapMaybe id
      -- -- & (\v -> trace (show ("depsExternDecls4" :: String, v)) v)
      & (id :: M.Map ModuleName BuildJobResult -> M.Map ModuleName BuildJobResult)
      & M.mapMaybe externFromBJRes
      -- & (\v -> trace (show ("depsExternDecls5" :: String, M.keys v)) v)
    )

  let (depsExternDecls :: M.Map ModuleName (M.Map B.ByteString [B.ByteString])) =
        (depsExternDeclsFromMVars <> depsExternsFromPrebuilts)
        & M.filterWithKey (\k _ -> elem k dirtyImportedModules)
        & (id :: M.Map ModuleName ExternsFile -> M.Map ModuleName ExternsFile)
        & fmap (bcCacheDeclarations . efBuildCache)
        -- & (\v -> trace (show ("depsExternDecls6" :: String, M.keys v)) v)
        & (id :: M.Map ModuleName (M.Map B.ByteString [B.ByteString]) -> M.Map ModuleName (M.Map B.ByteString [B.ByteString]))

  -- (\res -> if depsExternDecls == dirtyCachedImports then res else trace (show ("isCacheHit cache miss" :: String, res, efModuleName dirtyExterns, ("deps-len", length deps), ("depsExternDecls" :: String, M.keys depsExternDecls), ("dirtyCachedImports" :: String, M.keys dirtyCachedImports))) res) <$>
  (pure $ depsExternDecls == dirtyCachedImports)

-- eqDeclRefIgnoringSourceSpan a b =
--   case (a, b) of
--     (TypeClassRef _ aname, TypeClassRef _ bname) -> aname == bname
--     (TypeOpRef _ aname, TypeOpRef _ bname) -> aname == bname
--     (ValueRef _ aname, ValueRef _ bname) -> aname == bname
--     (ValueOpRef _ aname, ValueOpRef _ bname) -> aname == bname
--
--     (TypeRef _ aname amname2, TypeRef _ bname bmname2) -> aname == bname && amname2 == bmname2
--     (TypeInstanceRef _ aname anamesrc, TypeInstanceRef _ bname bnamesrc) -> aname == bname && anamesrc == bnamesrc
--
--     -- TODO[drathier]: handle re-exports; did the referenced module change?
--     -- | exporting everything imported from a module
--     (ModuleRef _ amoduname, ModuleRef _ bmoduname) -> amoduname == bmoduname
--      -- | exporting something from another module
--     (ReExportRef _ _ adeclref, ReExportRef _ _ bdeclref) -> eqDeclRefIgnoringSourceSpan adeclref bdeclref

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
  -> m (Maybe (MultipleErrors, ExternsFile, WasRebuildNeeded))
getResult buildPlan moduleName =
  case M.lookup moduleName (bpPrebuilt buildPlan) of
    Just es ->
      pure (Just (MultipleErrors [], pbExternsFile es, RebuildWasNotNeeded))
    Nothing -> do
      let bj = fromMaybe (internalError "make: no barrier") $ M.lookup moduleName (bpBuildJobs buildPlan)
      r <- readMVar $ bjResult bj
      pure $ buildJobSuccess r

-- | Gets the Prebuilt for any modules whose source files didn't change.
didModuleSourceFilesChange
  :: BuildPlan
  -> ModuleName
  -> Maybe Prebuilt
didModuleSourceFilesChange buildPlan moduleName =
  bjPrebuilt =<< M.lookup moduleName (bpBuildJobs buildPlan)

-- | Gets the Prebuilt for any modules whose source files didn't change.
getDirtyCacheFile
  :: BuildPlan
  -> ModuleName
  -> Maybe ExternsFile
getDirtyCacheFile buildPlan moduleName =
  bjDirtyExterns =<< M.lookup moduleName (bpBuildJobs buildPlan)

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
  _ <- trace (show ("BuildPlan.construct 1 start" :: String, unsafePerformIO dt)) $ pure ()
  let sortedModuleNames = map (getModuleName . CST.resPartial) sorted
  _ <- trace (show ("BuildPlan.construct 2 start" :: String, unsafePerformIO dt)) $ pure ()
  rebuildStatuses <- A.forConcurrently sortedModuleNames getRebuildStatus
  _ <- trace (show ("BuildPlan.construct 3 start" :: String, unsafePerformIO dt)) $ pure ()
  let prebuilt = mempty
        -- foldl' collectPrebuiltModules M.empty $
        --   mapMaybe (\s -> (statusModuleName s, statusRebuildNever s,) <$> statusPrebuilt s) (snd <$> rebuildStatuses)
  let toBeRebuilt = filter (not . flip M.member prebuilt . fst) rebuildStatuses
  _ <- trace (show ("BuildPlan.construct 4 start" :: String, unsafePerformIO dt)) $ pure ()
  buildJobs <- foldM makeBuildJob M.empty toBeRebuilt
  _ <- trace (show ("BuildPlan.construct 5 start" :: String, unsafePerformIO dt)) $ pure ()
  env <- C.newMVar primEnv
  _ <- trace (show ("BuildPlan.construct 6 start" :: String, unsafePerformIO dt)) $ pure ()
  res <- pure
    ( BuildPlan prebuilt buildJobs env
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
--     getRebuildStatus moduleName = (moduleName,) <$> do
--           -- prebuilt <- findExistingExtern moduleName
--           let dirtyExterns = pbExternsFile <$> prebuilt
--           let prebuilt = Prebuilt <$> pure (UTCTime (fromOrdinalDate 0 1) (fromInteger 0)) <*> dirtyExterns
--           pure (RebuildStatus
--             { statusModuleName = moduleName
--             , statusRebuildNever = True
--             , statusPrebuilt = prebuilt
--             , statusDirtyExterns = dirtyExterns
--             , statusNewCacheInfo = Nothing
--             })
--
    -- TODO[drathier]: statusDirtyExterns seemingly contains no more info than Prebuilt does; are we filtering Prebuilt but not DirtyExterns somewhere? Why have both?
    getRebuildStatus moduleName = (moduleName,) <$> do
      inputInfo <- getInputTimestampsAndHashes moduleName
      case inputInfo of
        Left RebuildNever -> do
          prebuilt <- findExistingExtern moduleName
          let dirtyExterns = pbExternsFile <$> prebuilt
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
          prebuilt <-
            -- NOTE[fh]: prebuilt is Nothing for source-modified files, and Just for non-source modified files
            if isUpToDate
              then findExistingExtern moduleName
              else pure Nothing
          let dirtyExterns = pbExternsFile <$> prebuilt
          pure (RebuildStatus
            { statusModuleName = moduleName
            , statusRebuildNever = False
            , statusPrebuilt = prebuilt
            , statusDirtyExterns = dirtyExterns
            , statusNewCacheInfo = Just newCacheInfo
            })

    findExistingExtern :: ModuleName -> m (Maybe Prebuilt)
    findExistingExtern moduleName = runMaybeT $ do
      timestamp <- MaybeT $ getOutputTimestamp moduleName
      externs <- MaybeT $ snd <$> readExterns moduleName
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

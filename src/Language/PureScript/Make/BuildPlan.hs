module Language.PureScript.Make.BuildPlan
  ( BuildPlan()
  , BuildJobResult(..)
  , buildJobSuccess
  , buildJobFailure
  , construct
  , getResult
  , collectResults
  , markComplete
  , needsRebuild
  ) where

import           Prelude

import           Control.Concurrent.Async.Lifted as A
import           Control.Concurrent.Lifted as C
import           Control.Monad hiding (sequence)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.Foldable (foldl')
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Time.Clock (UTCTime)
import           Language.PureScript.AST
import           Language.PureScript.Crash
import qualified Language.PureScript.CST as CST
import           Language.PureScript.Errors
import           Language.PureScript.Externs
import           Language.PureScript.Make.Actions as Actions
import           Language.PureScript.Names (ModuleName)

-- | The BuildPlan tracks information about our build progress, and holds all
-- prebuilt modules for incremental builds.
data BuildPlan = BuildPlan
  { bpPrebuilt :: M.Map ModuleName Prebuilt
  , bpBuildJobs :: M.Map ModuleName BuildJob
  }

data Prebuilt = Prebuilt
  { pbModificationTime :: UTCTime
  , pbExternsFile :: ExternsFile
  }

newtype BuildJob = BuildJob
  { bjResult :: C.MVar BuildJobResult
    -- ^ Note: an empty MVar indicates that the build job has not yet finished.
  }

data BuildJobResult
  = BuildJobSucceeded !MultipleErrors !ExternsFile
  -- ^ Succeeded, with warnings and externs
  --
  | BuildJobFailed !MultipleErrors
  -- ^ Failed, with errors

  | BuildJobSkipped
  -- ^ The build job was not run, because an upstream build job failed

buildJobSuccess :: BuildJobResult -> Maybe (MultipleErrors, ExternsFile)
buildJobSuccess (BuildJobSucceeded warnings externs) = Just (warnings, externs)
buildJobSuccess _ = Nothing

buildJobFailure :: BuildJobResult -> Maybe MultipleErrors
buildJobFailure (BuildJobFailed errors) = Just errors
buildJobFailure _ = Nothing

-- | Called when we finished compiling a module and want to report back the
-- compilation result, as well as any potential errors that were thrown.
markComplete
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> ModuleName
  -> BuildJobResult
  -> m ()
markComplete buildPlan moduleName result = do
  let BuildJob rVar = fromMaybe (internalError "make: markComplete no barrier") $ M.lookup moduleName (bpBuildJobs buildPlan)
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
  let prebuiltResults = M.map (BuildJobSucceeded (MultipleErrors []) . pbExternsFile) (bpPrebuilt buildPlan)
  barrierResults <- traverse (readMVar . bjResult) $ bpBuildJobs buildPlan
  pure (M.union prebuiltResults barrierResults)

-- | Gets the the build result for a given module name independent of whether it
-- was rebuilt or prebuilt. Prebuilt modules always return no warnings.
getResult
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> ModuleName
  -> m (Maybe (MultipleErrors, ExternsFile))
getResult buildPlan moduleName =
  case M.lookup moduleName (bpPrebuilt buildPlan) of
    Just es ->
      pure (Just (MultipleErrors [], pbExternsFile es))
    Nothing -> do
      r <- readMVar $ bjResult $ fromMaybe (internalError "make: no barrier") $ M.lookup moduleName (bpBuildJobs buildPlan)
      pure $ buildJobSuccess r

-- | Constructs a BuildPlan for the given module graph.
--
-- The given MakeActions are used to collect various timestamps in order to
-- determine whether a module needs rebuilding.
construct
  :: forall m. (Monad m, MonadBaseControl IO m)
  => MakeActions m
  -> ([CST.PartialResult Module], [(ModuleName, [ModuleName])])
  -> m BuildPlan
construct MakeActions{..} (sorted, graph) = do
  prebuilt <- foldl' collectPrebuiltModules M.empty . catMaybes <$> A.forConcurrently sorted findExistingExtern
  let toBeRebuilt = filter (not . flip M.member prebuilt . getModuleName . CST.resPartial) sorted
  buildJobs <- foldM makeBuildJob M.empty (map (getModuleName . CST.resPartial) toBeRebuilt)
  pure $ BuildPlan prebuilt buildJobs
  where
    makeBuildJob prev moduleName = do
      buildJob <- BuildJob <$> C.newEmptyMVar
      pure (M.insert moduleName buildJob prev)

    findExistingExtern :: CST.PartialResult Module -> m (Maybe (ModuleName, Bool, Prebuilt))
    findExistingExtern (getModuleName . CST.resPartial -> moduleName) = runMaybeT $ do
      inputTimestamp <- lift $ getInputTimestamp moduleName
      (rebuildNever, existingTimestamp) <-
        case inputTimestamp of
          Left RebuildNever ->
            fmap (True,) $ MaybeT $ getOutputTimestamp moduleName
          Right (Just t1) -> do
            outputTimestamp <- MaybeT $ getOutputTimestamp moduleName
            guard (t1 < outputTimestamp)
            pure (False, outputTimestamp)
          _ -> mzero
      externsFile <- MaybeT $ decodeExterns . snd <$> readExterns moduleName
      pure (moduleName, rebuildNever, Prebuilt existingTimestamp externsFile)

    collectPrebuiltModules :: M.Map ModuleName Prebuilt -> (ModuleName, Bool, Prebuilt) -> M.Map ModuleName Prebuilt
    collectPrebuiltModules prev (moduleName, rebuildNever, pb)
      | rebuildNever = M.insert moduleName pb prev
      | otherwise = do
          let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup moduleName graph)
          case traverse (fmap pbModificationTime . flip M.lookup prev) deps of
            Nothing ->
              -- If we end up here, one of the dependencies didn't exist in the
              -- prebuilt map and so we know a dependency needs to be rebuilt, which
              -- means we need to be rebuilt in turn.
              prev
            Just modTimes ->
              case maximumMaybe modTimes of
                Just depModTime | pbModificationTime pb < depModTime ->
                  prev
                _ -> M.insert moduleName pb prev

maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just $ maximum xs

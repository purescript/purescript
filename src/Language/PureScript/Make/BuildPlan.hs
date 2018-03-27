module Language.PureScript.Make.BuildPlan
  ( BuildPlan()
  , construct
  , getResult
  , collectErrors
  , collectResults
  , markComplete
  , needsRebuild
  ) where

import           Prelude

import           Control.Concurrent.Lifted as C
import           Control.Monad hiding (sequence)
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import           Data.Aeson (decode)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Version (showVersion)
import           Language.PureScript.AST
import           Language.PureScript.Crash
import           Language.PureScript.Errors
import           Language.PureScript.Externs
import           Language.PureScript.Make.Actions as Actions
import           Language.PureScript.Names (ModuleName)
import qualified Paths_purescript as Paths

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

data BuildJob = BuildJob
  { bjResult :: C.MVar (Maybe (MultipleErrors, ExternsFile))
  , bjErrors :: C.MVar (Maybe MultipleErrors)
  }

-- | Called when we finished compiling a module and want to report back the
-- compilation result, as well as any potential errors that were thrown.
markComplete
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> ModuleName
  -> Maybe (MultipleErrors, ExternsFile)
  -> Maybe MultipleErrors
  -> m ()
markComplete buildPlan moduleName result errors = do
  let BuildJob rVar eVar = fromMaybe (internalError "make: markComplete no barrier") $ M.lookup moduleName (bpBuildJobs buildPlan)
  putMVar rVar result
  putMVar eVar errors

-- | Whether or not the module with the given ModuleName needs to be rebuilt
needsRebuild :: BuildPlan -> ModuleName -> Bool
needsRebuild bp moduleName = M.member moduleName (bpBuildJobs bp)

-- | Collects errors for all modules that have been rebuilt. This will block
-- until all outstanding build jobs are finished.
collectErrors
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> m [MultipleErrors]
collectErrors buildPlan  = do
  errors <- traverse readMVar $ map bjErrors $ M.elems (bpBuildJobs buildPlan)
  pure (catMaybes errors)

-- | Collects ExternsFiles for all prebuilt as well as rebuilt modules. Panics
-- if any build job returned an error.
collectResults
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> m (M.Map ModuleName ExternsFile)
collectResults buildPlan = do
  let externs = M.map pbExternsFile (bpPrebuilt buildPlan)
  barrierResults <- traverse (takeMVar . bjResult) $ bpBuildJobs buildPlan
  let barrierExterns = M.map (snd . fromMaybe (internalError "make: externs were missing but no errors reported.")) barrierResults
  pure (M.union externs barrierExterns)

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
    Nothing ->
      readMVar $ bjResult $ fromMaybe (internalError "make: no barrier") $ M.lookup moduleName (bpBuildJobs buildPlan)

-- | Constructs a BuildPlan for the given module graph.
--
-- The given MakeActions are used to collect various timestamps in order to
-- determine whether a module needs rebuilding.
construct
  :: forall m. (Monad m, MonadBaseControl IO m)
  => MakeActions m
  -> ([Module], [(ModuleName, [ModuleName])])
  -> m BuildPlan
construct MakeActions{..} (sorted, graph) = do
  prebuilt <- foldM findExistingExtern M.empty sorted
  let toBeRebuilt = filter (not . flip M.member prebuilt . getModuleName) sorted
  buildJobs <- foldM makeBuildJob M.empty (map getModuleName toBeRebuilt)
  pure $ BuildPlan prebuilt buildJobs
  where
    makeBuildJob prev moduleName = do
      buildJob <- BuildJob <$> C.newEmptyMVar <*> C.newEmptyMVar
      pure (M.insert moduleName buildJob prev)

    findExistingExtern :: M.Map ModuleName Prebuilt -> Module -> m (M.Map ModuleName Prebuilt)
    findExistingExtern prev (getModuleName -> moduleName) = do
      outputTimestamp <- getOutputTimestamp moduleName
      let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup moduleName graph)
      case traverse (fmap pbModificationTime . flip M.lookup prev) deps of
        Nothing ->
          -- If we end up here, one of the dependencies didn't exist in the
          -- prebuilt map and so we know a dependency needs to be rebuilt, which
          -- means we need to be rebuilt in turn.
          pure prev
        Just modTimes -> do
          let dependencyTimestamp = maximumMaybe modTimes
          inputTimestamp <- getInputTimestamp moduleName
          let
            existingExtern = case (inputTimestamp, dependencyTimestamp, outputTimestamp) of
              (Right (Just t1), Just t3, Just t2) ->
                if t1 > t2 || t3 > t2 then Nothing else Just t2
              (Right (Just t1), Nothing, Just t2) ->
                if t1 > t2 then Nothing else Just t2
              (Left RebuildNever, _, Just t2) ->
                Just t2
              _ ->
                Nothing
          case existingExtern of
            Nothing -> pure prev
            Just outputTime -> do
              mexts <- decodeExterns . snd <$> readExterns moduleName
              case mexts of
                Just exts ->
                  pure (M.insert moduleName (Prebuilt outputTime exts) prev)
                Nothing -> pure prev

maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just $ maximum xs

decodeExterns :: Externs -> Maybe ExternsFile
decodeExterns bs = do
  externs <- decode bs
  guard $ T.unpack (efVersion externs) == showVersion Paths.version
  return externs

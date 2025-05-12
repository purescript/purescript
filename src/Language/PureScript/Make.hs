module Language.PureScript.Make
  ( make
  , make_
  , rebuildModule
  , rebuildModule'
  , inferForeignModules
  , module Monad
  , module Actions
  ) where

import Prelude

import Control.Concurrent.Lifted as C
import Control.DeepSeq (force)
import Control.Exception.Lifted (onException, bracket_, evaluate)
import Control.Monad (foldM, unless, void, when, (<=<))
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Supply (evalSupplyT, runSupply, runSupplyT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.State (runStateT)
import Control.Monad.Writer.Class (MonadWriter(..), censor)
import Control.Monad.Writer.Strict (runWriterT)
import Data.Function (on)
import Data.Foldable (fold, for_)
import Data.List (foldl', sortOn)
import Data.List.NonEmpty qualified as NEL
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (traceMarkerIO)
import Language.PureScript.AST (ErrorMessageHint(..), Module(..), SourceSpan(..), getModuleName, getModuleSourceSpan, importPrim)
import Language.PureScript.Crash (internalError)
import Language.PureScript.CST qualified as CST
import Language.PureScript.Docs.Convert qualified as Docs
import Language.PureScript.Environment (initEnvironment)
import Language.PureScript.Errors (MultipleErrors(..), SimpleErrorMessage(..), addHint, defaultPPEOptions, errorMessage', errorMessage'', prettyPrintMultipleErrors)
import Language.PureScript.Externs (ExternsFile, applyExternsFileToEnvironment, moduleToExternsFile)
import Language.PureScript.Linter (Name(..), lint, lintImports)
import Language.PureScript.ModuleDependencies (DependencyDepth(..), moduleSignature, sortModules)
import Language.PureScript.Names (ModuleName(..), isBuiltinModuleName, runModuleName)
import Language.PureScript.Renamer (renameInModule)
import Language.PureScript.Sugar (Env, collapseBindingGroups, createBindingGroups, desugar, desugarCaseGuards, externsEnv, primEnv)
import Language.PureScript.TypeChecker (CheckState(..), emptyCheckState, typeCheckModule)
import Language.PureScript.Make.BuildPlan (BuildJobResult(..), BuildPlan(..), getResult, isUpToDate)
import Language.PureScript.Make.BuildPlan qualified as BuildPlan
import Language.PureScript.Make.ExternsDiff (checkDiffs, emptyDiff, diffExterns)
import Language.PureScript.Make.Cache qualified as Cache
import Language.PureScript.Make.Actions as Actions
import Language.PureScript.Make.Monad as Monad
    ( Make(..),
      writeTextFile,
      writeJSONFile,
      writeCborFileIO,
      writeCborFile,
      setTimestamp,
      runMake,
      readTextFile,
      readJSONFileIO,
      readJSONFile,
      readExternsFile,
      readCborFileIO,
      readCborFile,
      makeIO,
      hashFile,
      getTimestampMaybe,
      getTimestamp,
      getCurrentTime,
      copyFile )
import Language.PureScript.CoreFn qualified as CF
import System.Directory (doesFileExist)
import System.FilePath (replaceExtension)
import Language.PureScript.TypeChecker.Monad (liftTypeCheckM)

-- | Rebuild a single module.
--
rebuildModule
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> [ExternsFile]
  -> Module
  -> m ExternsFile
rebuildModule actions externs m = do
  env <- fmap fst . runWriterT $ foldM externsEnv primEnv externs
  rebuildModule' actions env externs m

rebuildModule'
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> Env
  -> [ExternsFile]
  -> Module
  -> m ExternsFile
rebuildModule' act env ext mdl = rebuildModuleWithIndex act env ext mdl Nothing

rebuildModuleWithIndex
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> Env
  -> [ExternsFile]
  -> Module
  -> Maybe (Int, Int)
  -> m ExternsFile
rebuildModuleWithIndex MakeActions{..} exEnv externs m@(Module _ _ moduleName _ _) moduleIndex = do
  progress $ CompilingModule moduleName moduleIndex
  let env = foldl' (flip applyExternsFileToEnvironment) initEnvironment externs
      withPrim = importPrim m
  lint withPrim

  ((Module ss coms _ elaborated exps, env'), nextVar) <- runSupplyT 0 $ do
    (desugared, (exEnv', usedImports)) <- runStateT (desugar externs withPrim) (exEnv, mempty)
    let modulesExports = (\(_, _, exports) -> exports) <$> exEnv'
    (checked, CheckState{..}) <- runStateT (liftTypeCheckM $ typeCheckModule modulesExports desugared) $ emptyCheckState env
    let usedImports' = foldl' (flip $ \(fromModuleName, newtypeCtorName) ->
          M.alter (Just . (fmap DctorName newtypeCtorName :) . fold) fromModuleName) usedImports checkConstructorImportsForCoercible
    -- Imports cannot be linted before type checking because we need to
    -- known which newtype constructors are used to solve Coercible
    -- constraints in order to not report them as unused.
    censor (addHint (ErrorInModule moduleName)) $ lintImports checked exEnv' usedImports'
    return (checked, checkEnv)

  -- desugar case declarations *after* type- and exhaustiveness checking
  -- since pattern guards introduces cases which the exhaustiveness checker
  -- reports as not-exhaustive.
  (deguarded, nextVar') <- runSupplyT nextVar $ do
    desugarCaseGuards elaborated

  regrouped <- createBindingGroups moduleName . collapseBindingGroups $ deguarded
  let mod' = Module ss coms moduleName regrouped exps
      corefn = CF.moduleToCoreFn env' mod'
      (optimized, nextVar'') = runSupply nextVar' $ CF.optimizeCoreFn corefn
      (renamedIdents, renamed) = renameInModule optimized
      exts = moduleToExternsFile mod' env' renamedIdents
  ffiCodegen renamed

  -- It may seem more obvious to write `docs <- Docs.convertModule m env' here,
  -- but I have not done so for two reasons:
  -- 1. This should never fail; any genuine errors in the code should have been
  -- caught earlier in this function. Therefore if we do fail here it indicates
  -- a bug in the compiler, which should be reported as such.
  -- 2. We do not want to perform any extra work generating docs unless the
  -- user has asked for docs to be generated.
  let docs = case Docs.convertModule externs exEnv env' m of
               Left errs -> internalError $
                 "Failed to produce docs for " ++ T.unpack (runModuleName moduleName)
                 ++ "; details:\n" ++ prettyPrintMultipleErrors defaultPPEOptions errs
               Right d -> d

  evalSupplyT nextVar'' $ codegen renamed docs exts
  return exts

data MakeOptions = MakeOptions
  { moCollectAllExterns :: Bool
  }

-- | Compiles in "make" mode, compiling each module separately to a @.js@ file
-- and an @externs.cbor@ file.
--
-- If timestamps or hashes have not changed, existing externs files can be used
-- to provide upstream modules' types without having to typecheck those modules
-- again.
--
-- It collects and returns externs for all modules passed.
make :: forall m. (MonadIO m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
     => MakeActions m
     -> [CST.PartialResult Module]
     -> m [ExternsFile]
make  = make' (MakeOptions {moCollectAllExterns = True})

-- | Compiles in "make" mode, compiling each module separately to a @.js@ file
-- and an @externs.cbor@ file.
--
-- This version of make returns nothing.
make_ :: forall m. (MonadIO m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
     => MakeActions m
     -> [CST.PartialResult Module]
     -> m ()
make_ ma ms = void $ make' (MakeOptions {moCollectAllExterns = False}) ma ms

make' :: forall m. (MonadIO m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
     => MakeOptions
     -> MakeActions m
     -> [CST.PartialResult Module]
     -> m [ExternsFile]
make' MakeOptions{..} ma@MakeActions{..} ms = do
  checkModuleNames
  cacheDb <- readCacheDb

  (sorted, graph) <- sortModules Transitive (moduleSignature . CST.resPartial) ms
  let opts = BuildPlan.Options {optPreloadAllExterns = moCollectAllExterns}
  (buildPlan, newCacheDb) <- BuildPlan.construct opts ma cacheDb (sorted, graph)

  -- Limit concurrent module builds to the number of capabilities as
  -- (by default) inferred from `+RTS -N -RTS` or set explicitly like `-N4`.
  -- This is to ensure that modules complete fully before moving on, to avoid
  -- holding excess memory during compilation from modules that were paused
  -- by the Haskell runtime.
  capabilities <- getNumCapabilities
  let concurrency = max 1 capabilities
  lock <- C.newQSem concurrency

  let sortedModuleNames = getModuleName . CST.resPartial <$> sorted
  let toBeRebuilt = filter (BuildPlan.needsRebuild buildPlan . getModuleName . CST.resPartial) sorted
  let totalModuleCount = length toBeRebuilt
  for_ toBeRebuilt $ \m -> fork $ do
    let moduleName = getModuleName . CST.resPartial $ m
    let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup moduleName graph)
    buildModule lock buildPlan moduleName totalModuleCount
      (spanName . getModuleSourceSpan . CST.resPartial $ m)
      (fst $ CST.resFull m)
      (fmap importPrim . snd $ CST.resFull m)
      (deps `inOrderOf` sortedModuleNames)

      -- Prevent hanging on other modules when there is an internal error
      -- (the exception is thrown, but other threads waiting on MVars are released)
      `onException` BuildPlan.markComplete buildPlan moduleName (BuildJobFailed mempty)

  -- Wait for all threads to complete, and collect results (and errors).
  (failures, successes) <-
    let
      splitResults = \case
        BuildJobSucceeded _ exts _ ->
          Right exts
        BuildJobFailed errs ->
          Left errs
        BuildJobSkipped ->
          Left mempty
    in
      M.mapEither splitResults <$> BuildPlan.collectResults buildPlan

  -- Write the updated build cache database to disk
  writeCacheDb $ Cache.removeModules (M.keysSet failures) newCacheDb

  writePackageJson

  -- If generating docs, also generate them for the Prim modules
  outputPrimDocs
  -- All threads have completed, rethrow any caught errors.
  let errors = M.elems failures
  unless (null errors) $ throwError (mconcat errors)

  -- Here we return all the ExternsFile in the ordering of the topological sort,
  -- so they can be folded into an Environment. This result is used in the tests
  -- and in PSCI.
  let lookupResult mn@(ModuleName name) =
        fromMaybe (internalError $ "make: module not found in results: " <> T.unpack name)
        $ M.lookup mn successes

  pure $
    if moCollectAllExterns then
      map lookupResult sortedModuleNames
    else
      mapMaybe (flip M.lookup successes) sortedModuleNames

  where
  checkModuleNames :: m ()
  checkModuleNames = checkNoPrim *> checkModuleNamesAreUnique

  checkNoPrim :: m ()
  checkNoPrim =
    for_ ms $ \m ->
      let mn = getModuleName $ CST.resPartial m
      in when (isBuiltinModuleName mn) $
           throwError
             . errorMessage' (getModuleSourceSpan $ CST.resPartial m)
             $ CannotDefinePrimModules mn

  checkModuleNamesAreUnique :: m ()
  checkModuleNamesAreUnique =
    for_ (findDuplicates (getModuleName . CST.resPartial) ms) $ \mss ->
      throwError . flip foldMap mss $ \ms' ->
        let mn = getModuleName . CST.resPartial . NEL.head $ ms'
        in errorMessage'' (fmap (getModuleSourceSpan . CST.resPartial) ms') $ DuplicateModule mn

  -- Find all groups of duplicate values in a list based on a projection.
  findDuplicates :: Ord b => (a -> b) -> [a] -> Maybe [NEL.NonEmpty a]
  findDuplicates f xs =
    case filter ((> 1) . length) . NEL.groupBy ((==) `on` f) . sortOn f $ xs of
      [] -> Nothing
      xss -> Just xss

  -- Sort a list so its elements appear in the same order as in another list.
  inOrderOf :: (Ord a) => [a] -> [a] -> [a]
  inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

  buildModule :: QSem -> BuildPlan -> ModuleName -> Int -> FilePath -> [CST.ParserWarning] -> Either (NEL.NonEmpty CST.ParserError) Module -> [ModuleName] -> m ()
  buildModule lock buildPlan moduleName cnt fp pwarnings mres deps = do
    result <- flip catchError (return . BuildJobFailed) $ do
      let pwarnings' = CST.toMultipleWarnings fp pwarnings
      tell pwarnings'
      m <- CST.unwrapParserError fp mres
      -- We need to wait for dependencies to be built, before checking if the current
      -- module should be rebuilt, so the first thing to do is to wait on the
      -- MVars for the module's dependencies.
      mexterns <- fmap unzip . sequence <$> traverse (getResult buildPlan) deps

      case mexterns of
        Just (_, depsDiffExterns) -> do
          let externs = fst <$> depsDiffExterns
          let prevResult = BuildPlan.getPrevResult buildPlan moduleName
          let depsDiffs = traverse snd depsDiffExterns
          let maySkipBuild moduleIndex
                -- We may skip built only for up-to-date modules.
                | Just (status, exts) <- prevResult
                , isUpToDate status
                -- Check if no dep's externs have changed. If any of the diffs
                -- is Nothing means we can not check and need to rebuild.
                , Just False <- checkDiffs m <$> depsDiffs = do
                  -- We should update modification times to mark existing
                  -- compilation results as actual. If it fails to update timestamp
                  -- on any of exiting codegen targets, it will run the build process.
                  updated <- updateOutputTimestamp moduleName
                  if updated then do
                    progress $ SkippingModule moduleName moduleIndex
                    pure $ Just (exts, MultipleErrors [], Just (emptyDiff moduleName))
                  else
                    pure Nothing
                | otherwise = pure Nothing

          -- We need to ensure that all dependencies have been included in Env.
          C.modifyMVar_ (bpEnv buildPlan) $ \env -> do
            let
              go :: Env -> ModuleName -> m Env
              go e dep = case lookup dep (zip deps externs) of
                Just exts
                  | not (M.member dep e) -> externsEnv e exts
                _ -> return e
            foldM go env deps
          env <- C.readMVar (bpEnv buildPlan)
          idx <- C.takeMVar (bpIndex buildPlan)
          C.putMVar (bpIndex buildPlan) (idx + 1)

          (exts, warnings, diff) <- do
            let doBuild = do
                -- Bracket all of the per-module work behind the semaphore, including
                -- forcing the result. This is done to limit concurrency and keep
                -- memory usage down; see comments above.
                  (exts, warnings) <- bracket_ (C.waitQSem lock) (C.signalQSem lock) $ do
                    -- Eventlog markers for profiling; see debug/eventlog.js
                    liftBase $ traceMarkerIO $ T.unpack (runModuleName moduleName) <> " start"
                    -- Force the externs and warnings to avoid retaining excess module
                    -- data after the module is finished compiling.
                    extsAndWarnings <- evaluate . force <=< listen $ do
                      rebuildModuleWithIndex ma env externs m (Just (idx, cnt))
                    liftBase $ traceMarkerIO $ T.unpack (runModuleName moduleName) <> " end"
                    return extsAndWarnings
                  let diff = diffExterns exts <$> (snd <$> prevResult) <*> depsDiffs
                  pure (exts, warnings, diff)
            maySkipBuild (Just (idx, cnt)) >>= maybe doBuild pure
          return $ BuildJobSucceeded (pwarnings' <> warnings) exts diff

        -- If we got Nothing for deps externs, that means one of the deps failed
        -- to compile. Though if we have a previous built result we will keep to
        -- avoid potentially unnecessary recompilation next time.
        Nothing -> return $
          case BuildPlan.getPrevResult buildPlan moduleName of
            Just (_, exts) ->
              BuildJobSucceeded (MultipleErrors []) exts (Just (emptyDiff moduleName))
            Nothing ->
              BuildJobSkipped

    BuildPlan.markComplete buildPlan moduleName result

-- | Infer the module name for a module by looking for the same filename with
-- a .js extension.
inferForeignModules
  :: forall m
   . MonadIO m
  => M.Map ModuleName (Either RebuildPolicy FilePath)
  -> m (M.Map ModuleName FilePath)
inferForeignModules =
    fmap (M.mapMaybe id) . traverse inferForeignModule
  where
    inferForeignModule :: Either RebuildPolicy FilePath -> m (Maybe FilePath)
    inferForeignModule (Left _) = return Nothing
    inferForeignModule (Right path) = do
      let jsFile = replaceExtension path "js"
      exists <- liftIO $ doesFileExist jsFile
      if exists
        then return (Just jsFile)
        else return Nothing

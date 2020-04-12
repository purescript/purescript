module Language.PureScript.Make
  (
  -- * Make API
  rebuildModule
  , make
  , inferForeignModules
  , module Monad
  , module Actions
  ) where

import           Prelude.Compat

import           Control.Concurrent.Lifted as C
import           Control.Monad hiding (sequence)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class
import           Control.Monad.Supply
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import           Control.Monad.Writer.Class (MonadWriter(..))
import           Control.Monad.Writer.Strict (runWriterT)
import           Data.Function (on)
import           Data.Foldable (for_)
import           Data.List (foldl', sortBy)
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Language.PureScript.AST
import           Language.PureScript.Crash
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.Docs.Convert as Docs
import           Language.PureScript.Environment
import           Language.PureScript.Errors
import           Language.PureScript.Externs
import           Language.PureScript.Linter
import           Language.PureScript.ModuleDependencies
import           Language.PureScript.Names
import           Language.PureScript.Renamer
import           Language.PureScript.Sugar
import           Language.PureScript.TypeChecker
import           Language.PureScript.Make.BuildPlan
import qualified Language.PureScript.Make.BuildPlan as BuildPlan
import qualified Language.PureScript.Make.Cache as Cache
import           Language.PureScript.Make.Actions as Actions
import           Language.PureScript.Make.Monad as Monad
import qualified Language.PureScript.CoreFn as CF
import           System.Directory (doesFileExist)
import           System.FilePath (replaceExtension)

-- | Rebuild a single module.
--
-- This function is used for fast-rebuild workflows (PSCi and psc-ide are examples).
rebuildModule
  :: forall m
   . (Monad m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> [ExternsFile]
  -> Module
  -> m ExternsFile
rebuildModule actions externs m = do
  env <- fmap fst . runWriterT $ foldM externsEnv primEnv externs
  rebuildModule' actions env externs m

rebuildModule'
  :: forall m
   . (Monad m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> Env
  -> [ExternsFile]
  -> Module
  -> m ExternsFile
rebuildModule' MakeActions{..} exEnv externs m@(Module _ _ moduleName _ _) = do
  progress $ CompilingModule moduleName
  let env = foldl' (flip applyExternsFileToEnvironment) initEnvironment externs
      withPrim = importPrim m
  lint withPrim
  ((Module ss coms _ elaborated exps, env'), nextVar) <- runSupplyT 0 $ do
    desugar exEnv externs [withPrim] >>= \case
      [desugared] -> runCheck' (emptyCheckState env) $ typeCheckModule desugared
      _ -> internalError "desugar did not return a singleton"

  -- desugar case declarations *after* type- and exhaustiveness checking
  -- since pattern guards introduces cases which the exhaustiveness checker
  -- reports as not-exhaustive.
  (deguarded, nextVar') <- runSupplyT nextVar $ do
    desugarCaseGuards elaborated

  regrouped <- createBindingGroups moduleName . collapseBindingGroups $ deguarded
  let mod' = Module ss coms moduleName regrouped exps
      corefn = CF.moduleToCoreFn env' mod'
      optimized = CF.optimizeCoreFn corefn
      [renamed] = renameInModules [optimized]
      exts = moduleToExternsFile mod' env'
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

  evalSupplyT nextVar' $ codegen renamed docs exts
  return exts

-- | Compiles in "make" mode, compiling each module separately to a @.js@ file and an @externs.cbor@ file.
--
-- If timestamps or hashes have not changed, existing externs files can be used to provide upstream modules' types without
-- having to typecheck those modules again.
make :: forall m. (Monad m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
     => MakeActions m
     -> [CST.PartialResult Module]
     -> m [ExternsFile]
make ma@MakeActions{..} ms = do
  checkModuleNames
  cacheDb <- readCacheDb

  (sorted, graph) <- sortModules (moduleSignature . CST.resPartial) ms

  (buildPlan, newCacheDb) <- BuildPlan.construct ma cacheDb (sorted, graph)

  let toBeRebuilt = filter (BuildPlan.needsRebuild buildPlan . getModuleName . CST.resPartial) sorted
  for_ toBeRebuilt $ \m -> fork $ do
    let moduleName = getModuleName . CST.resPartial $ m
    let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup moduleName graph)
    buildModule buildPlan moduleName
      (spanName . getModuleSourceSpan . CST.resPartial $ m)
      (importPrim <$> CST.resFull m)
      (deps `inOrderOf` map (getModuleName . CST.resPartial) sorted)

  -- Wait for all threads to complete, and collect results (and errors).
  (failures, successes) <-
    let
      splitResults = \case
        BuildJobSucceeded _ exts ->
          Right exts
        BuildJobFailed errs ->
          Left errs
        BuildJobSkipped ->
          Left mempty
    in
      M.mapEither splitResults <$> BuildPlan.collectResults buildPlan

  -- Write the updated build cache database to disk
  writeCacheDb $ Cache.removeModules (M.keysSet failures) newCacheDb

  -- If generating docs, also generate them for the Prim modules
  outputPrimDocs

  -- All threads have completed, rethrow any caught errors.
  let errors = M.elems failures
  unless (null errors) $ throwError (mconcat errors)

  -- Here we return all the ExternsFile in the ordering of the topological sort,
  -- so they can be folded into an Environment. This result is used in the tests
  -- and in PSCI.
  let lookupResult mn =
        fromMaybe (internalError "make: module not found in results")
        $ M.lookup mn successes
  return (map (lookupResult . getModuleName . CST.resPartial) sorted)

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
    case filter ((> 1) . length) . NEL.groupBy ((==) `on` f) . sortBy (compare `on` f) $ xs of
      [] -> Nothing
      xss -> Just xss

  -- Sort a list so its elements appear in the same order as in another list.
  inOrderOf :: (Ord a) => [a] -> [a] -> [a]
  inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

  buildModule :: BuildPlan -> ModuleName -> FilePath -> Either (NEL.NonEmpty CST.ParserError) Module -> [ModuleName] -> m ()
  buildModule buildPlan moduleName fp mres deps = do
    result <- flip catchError (return . BuildJobFailed) $ do
      m <- CST.unwrapParserError fp mres
      -- We need to wait for dependencies to be built, before checking if the current
      -- module should be rebuilt, so the first thing to do is to wait on the
      -- MVars for the module's dependencies.
      mexterns <- fmap unzip . sequence <$> traverse (getResult buildPlan) deps

      case mexterns of
        Just (_, externs) -> do
          -- We need to ensure that all dependencies have been included in Env
          C.modifyMVar_ (bpEnv buildPlan) $ \env -> do
            let
              go :: Env -> ModuleName -> m Env
              go e dep = case lookup dep (zip deps externs) of
                Just exts
                  | not (M.member dep e) -> externsEnv e exts
                _ -> return e
            foldM go env deps
          env <- C.readMVar (bpEnv buildPlan)
          (exts, warnings) <- listen $ rebuildModule' ma env externs m
          return $ BuildJobSucceeded warnings exts
        Nothing -> return BuildJobSkipped

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

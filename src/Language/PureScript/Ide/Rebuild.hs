{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE TemplateHaskell       #-}

module Language.PureScript.Ide.Rebuild
  ( rebuildFileSync
  , rebuildFileAsync
  , rebuildFile
  , specialCompletion
  ) where

import           Protolude

import           Data.Char
import           "monad-logger" Control.Monad.Logger
import qualified Data.List                       as List
import qualified Data.Map.Lazy                   as M
import           Data.Maybe                      (fromJust)
import qualified Data.Set                        as S
import qualified Data.Text                       as T
import qualified Language.PureScript             as P
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Logging
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

-- | Given a filepath performs the following steps:
--
-- * Reads and parses a PureScript module from the filepath.
--
-- * Builds a dependency graph for the parsed module from the already loaded
-- ExternsFiles.
--
-- * Attempts to find an FFI definition file for the module by looking
-- for a file with the same filepath except for a .js extension.
--
-- * Passes all the created artifacts to @rebuildModule@.
--
-- * If the rebuilding succeeds, returns a @RebuildSuccess@ with the generated
-- warnings, and if rebuilding fails, returns a @RebuildError@ with the
-- generated errors.
rebuildFile
  :: (Ide m, MonadLogger m, MonadError IdeError m)
  => FilePath
  -- ^ The file to rebuild
  -> (ReaderT IdeEnvironment (LoggingT IO) () -> m ())
  -- ^ A runner for the second build with open exports
  -> m Success
rebuildFile path =
  ideReadFile path >>= rebuildFile' path

rebuildFile'
  :: (Ide m, MonadLogger m, MonadError PscIdeError m)
  => FilePath
  -> Text
  -> m Success
rebuildFile' path input = do

  m <- case snd <$> P.parseModuleFromFile identity (path, input) of
    Left parseError ->
      throwError (RebuildError
                  (P.MultipleErrors [P.toPositionedError parseError]))
    Right m -> pure m

  -- Externs files must be sorted ahead of time, so that they get applied
  -- correctly to the 'Environment'.
  externs <- logPerf (labelTimespec "Sorting externs") (sortExterns m =<< getExternFiles)

  outputDirectory <- confOutputPath . ideConfiguration <$> ask

  -- For rebuilding, we want to 'RebuildAlways', but for inferring foreign
  -- modules using their file paths, we need to specify the path in the 'Map'.
  let filePathMap = M.singleton (P.getModuleName m) (Left P.RebuildAlways)
  foreigns <-
    P.inferForeignModules (M.singleton (P.getModuleName m) (Right path))

  let makeEnv = MakeActionsEnv outputDirectory filePathMap foreigns False
  -- Rebuild the single module using the cached externs
  (result, warnings) <- logPerf (labelTimespec "Rebuilding Module") $
    liftIO
    . P.runMake P.defaultOptions
    . P.rebuildModule (buildMakeActions
                        >>= shushProgress $ makeEnv) externs $ m
  case result of
    Left errors -> throwError (RebuildError errors)
    Right _ -> do
      runOpenBuild (rebuildModuleOpen makeEnv externs m)
      pure (RebuildSuccess warnings)

rebuildFileAsync
  :: forall m. (Ide m, MonadLogger m, MonadError IdeError m)
  => FilePath -> m Success
rebuildFileAsync fp = rebuildFile fp asyncRun
  where
    asyncRun :: ReaderT IdeEnvironment (LoggingT IO) () -> m ()
    asyncRun action = do
        env <- ask
        let ll = confLogLevel (ideConfiguration env)
        void (liftIO (async (runLogger ll (runReaderT action env))))

rebuildFileSync
  :: forall m. (Ide m, MonadLogger m, MonadError IdeError m)
  => FilePath -> m Success
rebuildFileSync fp = rebuildFile fp syncRun
  where
    syncRun :: ReaderT IdeEnvironment (LoggingT IO) () -> m ()
    syncRun action = do
        env <- ask
        let ll = confLogLevel (ideConfiguration env)
        void (liftIO (runLogger ll (runReaderT action env)))


-- | Rebuilds a module but opens up its export list first and stores the result
-- inside the rebuild cache
rebuildModuleOpen
  :: (Ide m, MonadLogger m)
  => MakeActionsEnv
  -> [P.ExternsFile]
  -> P.Module
  -> m ()
rebuildModuleOpen makeEnv externs m = void $ runExceptT $ do
  (openResult, _) <- liftIO
    . P.runMake P.defaultOptions
    . P.rebuildModule (buildMakeActions
                       >>= shushProgress
                       >>= shushCodegen
                       $ makeEnv) externs $ openModuleExports m
  case openResult of
    Left _ ->
      throwError (GeneralError "Failed when rebuilding with open exports")
    Right result -> do
      $(logDebug)
        ("Setting Rebuild cache: " <> P.runModuleName (P.efModuleName result))
      cacheRebuild result

-- | Parameters we can access while building our @MakeActions@
data MakeActionsEnv =
  MakeActionsEnv
  { maeOutputDirectory :: FilePath
  , maeFilePathMap     :: ModuleMap (Either P.RebuildPolicy FilePath)
  , maeForeignPathMap  :: ModuleMap FilePath
  , maePrefixComment   :: Bool
  }

-- | Builds the default @MakeActions@ from a @MakeActionsEnv@
buildMakeActions :: MakeActionsEnv -> P.MakeActions P.Make
buildMakeActions MakeActionsEnv{..} =
  P.buildMakeActions
    maeOutputDirectory
    maeFilePathMap
    maeForeignPathMap
    maePrefixComment

-- | Shuts the compiler up about progress messages
shushProgress :: P.MakeActions P.Make -> MakeActionsEnv -> P.MakeActions P.Make
shushProgress ma _ =
  ma { P.progress = \_ -> pure () }

-- | Stops any kind of codegen (also silences errors about missing or unused FFI
-- files though)
shushCodegen :: P.MakeActions P.Make -> MakeActionsEnv -> P.MakeActions P.Make
shushCodegen ma MakeActionsEnv{..} =
  ma { P.codegen = \_ _ _ -> pure () }

-- | Returns a topologically sorted list of dependent ExternsFiles for the given
-- module. Throws an error if there is a cyclic dependency within the
-- ExternsFiles
sortExterns
  :: (Ide m, MonadError IdeError m)
  => P.Module
  -> ModuleMap P.ExternsFile
  -> m [P.ExternsFile]
sortExterns m ex = do
  sorted' <- runExceptT
           . P.sortModules
           . (:) m
           . map mkShallowModule
           . M.elems
           . M.delete (P.getModuleName m) $ ex
  case sorted' of
    Left err -> throwError (RebuildError err)
    Right (sorted, graph) -> do
      let deps = fromJust (List.lookup (P.getModuleName m) graph)
      pure $ mapMaybe getExtern (deps `inOrderOf` map P.getModuleName sorted)
  where
    mkShallowModule P.ExternsFile{..} =
      P.Module (P.internalModuleSourceSpan "<rebuild>") [] efModuleName (map mkImport efImports) Nothing
    mkImport (P.ExternsImport mn it iq) =
      P.ImportDeclaration mn it iq
    getExtern mn = M.lookup mn ex
    -- Sort a list so its elements appear in the same order as in another list.
    inOrderOf :: (Ord a) => [a] -> [a] -> [a]
    inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

-- | Removes a modules export list.
openModuleExports :: P.Module -> P.Module
openModuleExports (P.Module ss cs mn decls _) = P.Module ss cs mn decls Nothing

specialCompletion
  :: (Ide m, MonadLogger m)
  => FilePath
  -> Int
  -> Int
  -> m [Text]
specialCompletion path row col = do
  input <- liftIO (readUTF8FileT path)
  let withHole = traceShowId (insertHole input)
  rebuildResult <- runExceptT (rebuildFile' path withHole)
  traceShowM rebuildResult
  case rebuildResult of
    Left (RebuildError errs)
      | Just holeError <- extractHole errs -> do
        traceShowM holeError
        pure (extractCompletions holeError)
    _ -> pure []

  where
    insertHole :: Text -> Text
    insertHole t =
      let
        (before, line:after) = splitAt (row - 1) (T.lines t)
        (b, a) = T.splitAt (col - 1) line
        (start, ident) = breakEnd (not . isSpace) b
        withHole = start <> " ( ?magicUnicornHole " <> ident <> ") " <> T.tail a
      in
        T.unlines (before <> [withHole] <> after)

    extractHole :: P.MultipleErrors -> Maybe P.Type
    extractHole me = asum (map unicornTypeHole (P.runMultipleErrors me))

    extractCompletions :: P.Type -> [Text]
    extractCompletions =
      map (P.prettyPrintStringJS . P.runLabel . fst)
      . fst
      . P.rowToList



breakEnd :: (Char -> Bool) -> Text -> (Text, Text)
breakEnd p t = (T.dropWhileEnd p t, T.takeWhileEnd p t)
    
unicornTypeHole :: P.ErrorMessage -> Maybe P.Type
unicornTypeHole (P.ErrorMessage _
                 (P.HoleInferredType "magicUnicornHole"
                  (P.TypeApp (P.TypeApp t' (P.TypeApp r t)) _) _ _))
  | t' == P.tyFunction && r == P.tyRecord = Just t
unicornTypeHole _ = Nothing

-- (TypeApp (TypeApp (TypeConstructor (Qualified (Just (ModuleName [ProperName {runProperName = "Prim"}])) (ProperName {runProperName = "Function"}))) (TypeApp (TypeConstructor (Qualified (Just (ModuleName [ProperName {runProperName = "Prim"}])) (ProperName {runProperName = "Record"}))) (RCons (Label {runLabel = "x"}) (TypeConstructor (Qualified (Just (ModuleName [ProperName {runProperName = "Prim"}])) (ProperName {runProperName = "Int"}))) (RCons (Label {runLabel = "y"}) (TypeConstructor (Qualified (Just (ModuleName [ProperName {runProperName = "Prim"}])) (ProperName {runProperName = "String"}))) REmpty)))) (TUnknown 3))

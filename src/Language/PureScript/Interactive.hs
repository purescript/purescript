{-# LANGUAGE DoAndIfThenElse #-}

module Language.PureScript.Interactive
  ( handleCommand
  , module Interactive

  -- TODO: remove these exports
  , make
  , runMake
  ) where

import           Prelude ()
import           Prelude.Compat
import           Protolude (ordNub)

import           Data.List (sort, find, foldl')
import           Data.Maybe (mapMaybe)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Class
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.State.Strict (StateT, runStateT, evalStateT)
import           Control.Monad.Writer.Strict (Writer(), runWriter)

import qualified Language.PureScript as P
import qualified Language.PureScript.Names as N

import           Language.PureScript.Interactive.Completion   as Interactive
import           Language.PureScript.Interactive.IO           as Interactive
import           Language.PureScript.Interactive.Message      as Interactive
import           Language.PureScript.Interactive.Module       as Interactive
import           Language.PureScript.Interactive.Parser       as Interactive
import           Language.PureScript.Interactive.Printer      as Interactive
import           Language.PureScript.Interactive.Types        as Interactive

import           System.Directory (getCurrentDirectory)
import           System.FilePath ((</>))
import           System.FilePath.Glob (glob)

-- | Pretty-print errors
printErrors :: MonadIO m => P.MultipleErrors -> m ()
printErrors errs = liftIO $ do
  pwd <- getCurrentDirectory
  putStrLn $ P.prettyPrintMultipleErrors P.defaultPPEOptions {P.ppeRelativeDirectory = pwd} errs

-- | This is different than the runMake in 'Language.PureScript.Make' in that it specifies the
-- options and ignores the warning messages.
runMake :: P.Make a -> IO (Either P.MultipleErrors a)
runMake mk = fst <$> P.runMake P.defaultOptions mk

-- | Rebuild a module, using the cached externs data for dependencies.
rebuild
  :: [P.ExternsFile]
  -> P.Module
  -> P.Make (P.ExternsFile, P.Environment)
rebuild loadedExterns m = do
    externs <- P.rebuildModule buildActions loadedExterns m
    return (externs, foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment (loadedExterns ++ [externs]))
  where
    buildActions :: P.MakeActions P.Make
    buildActions =
      (P.buildMakeActions modulesDir
                          filePathMap
                          M.empty
                          False) { P.progress = const (return ()) }

    filePathMap :: M.Map P.ModuleName (Either P.RebuildPolicy FilePath)
    filePathMap = M.singleton (P.getModuleName m) (Left P.RebuildAlways)

-- | Build the collection of modules from scratch. This is usually done on startup.
make
  :: [(FilePath, P.Module)]
  -> P.Make ([P.ExternsFile], P.Environment)
make ms = do
    foreignFiles <- P.inferForeignModules filePathMap
    externs <- P.make (buildActions foreignFiles) (map snd ms)
    return (externs, foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment externs)
  where
    buildActions :: M.Map P.ModuleName FilePath -> P.MakeActions P.Make
    buildActions foreignFiles =
      P.buildMakeActions modulesDir
                         filePathMap
                         foreignFiles
                         False

    filePathMap :: M.Map P.ModuleName (Either P.RebuildPolicy FilePath)
    filePathMap = M.fromList $ map (\(fp, m) -> (P.getModuleName m, Right fp)) ms

-- | Performs a PSCi command
handleCommand
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => (String -> m ()) -- ^ evaluate JS
  -> m () -- ^ reload
  -> (String -> m ()) -- ^ print into console
  -> Command
  -> m ()
handleCommand _ _ p ShowHelp                  = p helpMessage
handleCommand _ r _ ReloadState               = handleReloadState r
handleCommand _ r _ ClearState                = handleClearState r
handleCommand e _ _ (Expression val)          = handleExpression e val
handleCommand _ _ _ (Import im)               = handleImport im
handleCommand _ _ _ (Decls l)                 = handleDecls l
handleCommand _ _ p (TypeOf val)              = handleTypeOf p val
handleCommand _ _ p (KindOf typ)              = handleKindOf p typ
handleCommand _ _ p (BrowseModule moduleName) = handleBrowse p moduleName
handleCommand _ _ p (ShowInfo QueryLoaded)    = handleShowLoadedModules p
handleCommand _ _ p (ShowInfo QueryImport)    = handleShowImportedModules p
handleCommand _ _ p (CompleteStr prefix)      = handleComplete p prefix
handleCommand _ _ _ _                         = P.internalError "handleCommand: unexpected command"

-- | Reload the application state
handleReloadState
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => m ()
  -> m ()
handleReloadState reload = do
  modify $ updateLets (const [])
  globs <- asks psciFileGlobs
  files <- liftIO $ concat <$> traverse glob globs
  e <- runExceptT $ do
    modules <- ExceptT . liftIO $ loadAllModules files
    (externs, _) <- ExceptT . liftIO . runMake . make $ modules
    return (map snd modules, externs)
  case e of
    Left errs -> printErrors errs
    Right (modules, externs) -> do
      modify (updateLoadedExterns (const (zip modules externs)))
      reload

-- | Clear the application state
handleClearState
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => m ()
  -> m ()
handleClearState reload = do
  modify $ updateImportedModules (const [])
  handleReloadState reload

-- | Takes a value expression and evaluates it with the current state.
handleExpression
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => (String -> m ())
  -> P.Expr
  -> m ()
handleExpression evaluate val = do
  st <- get
  let m = createTemporaryModule True st val
  e <- liftIO . runMake $ rebuild (map snd (psciLoadedExterns st)) m
  case e of
    Left errs -> printErrors errs
    Right _ -> do
      js <- liftIO $ readFile (modulesDir </> "$PSCI" </> "index.js")
      evaluate js

-- |
-- Takes a list of declarations and updates the environment, then run a make. If the declaration fails,
-- restore the original environment.
--
handleDecls
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => [P.Declaration]
  -> m ()
handleDecls ds = do
  st <- gets (updateLets (++ ds))
  let m = createTemporaryModule False st (P.Literal (P.ObjectLiteral []))
  e <- liftIO . runMake $ rebuild (map snd (psciLoadedExterns st)) m
  case e of
    Left err -> printErrors err
    Right _ -> put st

-- | Show actual loaded modules in psci.
handleShowLoadedModules
  :: (MonadState PSCiState m, MonadIO m)
  => (String -> m ())
  -> m ()
handleShowLoadedModules print' = do
    loadedModules <- gets psciLoadedExterns
    print' $ readModules loadedModules
  where
    readModules = unlines . sort . ordNub . map (T.unpack . P.runModuleName . P.getModuleName . fst)

-- | Show the imported modules in psci.
handleShowImportedModules
  :: (MonadState PSCiState m, MonadIO m)
  => (String -> m ())
  -> m ()
handleShowImportedModules print' = do
  PSCiState { psciImportedModules = importedModules } <- get
  print' $ showModules importedModules
  return ()
  where
  showModules = unlines . sort . map (T.unpack . showModule)
  showModule (mn, declType, asQ) =
    "import " <> N.runModuleName mn <> showDeclType declType <>
    foldMap (\mn' -> " as " <> N.runModuleName mn') asQ

  showDeclType P.Implicit = ""
  showDeclType (P.Explicit refs) = refsList refs
  showDeclType (P.Hiding refs) = " hiding " <> refsList refs
  refsList refs = " (" <> commaList (mapMaybe showRef refs) <> ")"

  showRef :: P.DeclarationRef -> Maybe Text
  showRef (P.TypeRef _ pn dctors) =
    Just $ N.runProperName pn <> "(" <> maybe ".." (commaList . map N.runProperName) dctors <> ")"
  showRef (P.TypeOpRef _ op) =
    Just $ "type " <> N.showOp op
  showRef (P.ValueRef _ ident) =
    Just $ N.runIdent ident
  showRef (P.ValueOpRef _ op) =
    Just $ N.showOp op
  showRef (P.TypeClassRef _ pn) =
    Just $ "class " <> N.runProperName pn
  showRef (P.TypeInstanceRef _ ident) =
    Just $ N.runIdent ident
  showRef (P.ModuleRef _ name) =
    Just $ "module " <> N.runModuleName name
  showRef (P.KindRef _ pn) =
    Just $ "kind " <> N.runProperName pn
  showRef (P.ReExportRef _ _ _) =
    Nothing

  commaList :: [Text] -> Text
  commaList = T.intercalate ", "

-- | Imports a module, preserving the initial state on failure.
handleImport
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => ImportedModule
  -> m ()
handleImport im = do
   st <- gets (updateImportedModules (im :))
   let m = createTemporaryModuleForImports st
   e <- liftIO . runMake $ rebuild (map snd (psciLoadedExterns st)) m
   case e of
     Left errs -> printErrors errs
     Right _  -> put st

-- | Takes a value and prints its type
handleTypeOf
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => (String -> m ())
  -> P.Expr
  -> m ()
handleTypeOf print' val = do
  st <- get
  let m = createTemporaryModule False st val
  e <- liftIO . runMake $ rebuild (map snd (psciLoadedExterns st)) m
  case e of
    Left errs -> printErrors errs
    Right (_, env') ->
      case M.lookup (P.mkQualified (P.Ident "it") (P.ModuleName [P.ProperName "$PSCI"])) (P.names env') of
        Just (ty, _, _) -> print' . P.prettyPrintType $ ty
        Nothing -> print' "Could not find type"

-- | Takes a type and prints its kind
handleKindOf
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => (String -> m ())
  -> P.Type
  -> m ()
handleKindOf print' typ = do
  st <- get
  let m = createTemporaryModuleForKind st typ
      mName = P.ModuleName [P.ProperName "$PSCI"]
  e <- liftIO . runMake $ rebuild (map snd (psciLoadedExterns st)) m
  case e of
    Left errs -> printErrors errs
    Right (_, env') ->
      case M.lookup (P.Qualified (Just mName) $ P.ProperName "IT") (P.typeSynonyms env') of
        Just (_, typ') -> do
          let chk = (P.emptyCheckState env') { P.checkCurrentModule = Just mName }
              k   = check (P.kindOf typ') chk

              check :: StateT P.CheckState (ExceptT P.MultipleErrors (Writer P.MultipleErrors)) a -> P.CheckState -> Either P.MultipleErrors (a, P.CheckState)
              check sew = fst . runWriter . runExceptT . runStateT sew
          case k of
            Left err        -> printErrors err
            Right (kind, _) -> print' . T.unpack . P.prettyPrintKind $ kind
        Nothing -> print' "Could not find kind"

-- | Browse a module and displays its signature
handleBrowse
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => (String -> m ())
  -> P.ModuleName
  -> m ()
handleBrowse print' moduleName = do
  st <- get
  env <- asks psciEnvironment
  if isModInEnv moduleName st
    then print' $ printModuleSignatures moduleName env
    else case lookupUnQualifiedModName moduleName st of
      Just unQualifiedName ->
        if isModInEnv unQualifiedName st
          then print' $ printModuleSignatures unQualifiedName env
          else failNotInEnv moduleName
      Nothing ->
        failNotInEnv moduleName
  where
    isModInEnv modName =
        any ((== modName) . P.getModuleName . fst) . psciLoadedExterns
    failNotInEnv modName =
        print' $ T.unpack $ "Module '" <> N.runModuleName modName <> "' is not valid."
    lookupUnQualifiedModName quaModName st =
        (\(modName,_,_) -> modName) <$> find ( \(_, _, mayQuaName) -> mayQuaName == Just quaModName) (psciImportedModules st)

-- | Return output as would be returned by tab completion, for tools integration etc.
handleComplete
  :: (MonadState PSCiState m, MonadIO m)
  => (String -> m ())
  -> String
  -> m ()
handleComplete print' prefix = do
  st <- get
  let act = liftCompletionM (completion' (reverse prefix, ""))
  results <- evalStateT act st
  print' $ unlines (formatCompletions results)

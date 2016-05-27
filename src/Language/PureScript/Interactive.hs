{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

module Language.PureScript.Interactive
  ( handleCommand
  , module Interactive

  -- TODO: remove these exports
  , make
  , runMake
  ) where

import           Prelude ()
import           Prelude.Compat

import           Data.List (intercalate, nub, sort, find, foldl')
import qualified Data.Map as M

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Class
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.State.Strict (StateT, runStateT)
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

import           System.Exit
import           System.Process (readProcessWithExitCode)

-- | Pretty-print errors
printErrors :: MonadIO m => P.MultipleErrors -> m ()
printErrors = liftIO . putStrLn . P.prettyPrintMultipleErrors False

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
  => Command
  -> m ()
handleCommand ShowHelp                  = liftIO $ putStrLn helpMessage
handleCommand ResetState                = handleResetState
handleCommand (Expression val)          = handleExpression val
handleCommand (Import im)               = handleImport im
handleCommand (Decls l)                 = handleDecls l
handleCommand (TypeOf val)              = handleTypeOf val
handleCommand (KindOf typ)              = handleKindOf typ
handleCommand (BrowseModule moduleName) = handleBrowse moduleName
handleCommand (ShowInfo QueryLoaded)    = handleShowLoadedModules
handleCommand (ShowInfo QueryImport)    = handleShowImportedModules
handleCommand QuitPSCi                  = P.internalError "`handleCommand QuitPSCi` was called. This is a bug."

-- | Reset the application state
handleResetState
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => m ()
handleResetState = do
  modify $ updateImportedModules (const [])
         . updateLets (const [])
  files <- asks psciLoadedFiles
  e <- runExceptT $ do
    modules <- ExceptT . liftIO $ loadAllModules files
    (externs, _) <- ExceptT . liftIO . runMake . make $ modules
    return (map snd modules, externs)
  case e of
    Left errs -> printErrors errs
    Right (modules, externs) -> modify (updateLoadedExterns (const (zip modules externs)))

-- | Takes a value expression and evaluates it with the current state.
--
-- TODO: factor out the Node process runner, so that we can use PSCi in other settings.
handleExpression
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => P.Expr
  -> m ()
handleExpression val = do
  st <- get
  let m = createTemporaryModule True st val
  nodeArgs <- asks ((++ [indexFile]) . psciNodeFlags)
  e <- liftIO . runMake $ rebuild (map snd (psciLoadedExterns st)) m
  case e of
    Left errs -> printErrors errs
    Right _ -> do
      liftIO $ writeFile indexFile "require('$PSCI')['$main']();"
      process <- liftIO findNodeProcess
      result  <- liftIO $ traverse (\node -> readProcessWithExitCode node nodeArgs "") process
      case result of
        Just (ExitSuccess,   out, _)   -> liftIO $ putStrLn out
        Just (ExitFailure _, _,   err) -> liftIO $ putStrLn err
        Nothing                        -> liftIO $ putStrLn "Couldn't find node.js"

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
  => m ()
handleShowLoadedModules = do
    loadedModules <- gets psciLoadedExterns
    liftIO $ putStrLn (readModules loadedModules)
  where
    readModules = unlines . sort . nub . map (P.runModuleName . P.getModuleName . fst)

-- | Show the imported modules in psci.
handleShowImportedModules
  :: (MonadState PSCiState m, MonadIO m)
  => m ()
handleShowImportedModules = do
  PSCiState { psciImportedModules = importedModules } <- get
  liftIO $ showModules importedModules >>= putStrLn
  return ()
  where
  showModules = return . unlines . sort . map showModule
  showModule (mn, declType, asQ) =
    "import " ++ N.runModuleName mn ++ showDeclType declType ++
    foldMap (\mn' -> " as " ++ N.runModuleName mn') asQ

  showDeclType P.Implicit = ""
  showDeclType (P.Explicit refs) = refsList refs
  showDeclType (P.Hiding refs) = " hiding " ++ refsList refs
  refsList refs = " (" ++ commaList (map showRef refs) ++ ")"

  showRef :: P.DeclarationRef -> String
  showRef (P.TypeRef pn dctors) = N.runProperName pn ++ "(" ++ maybe ".." (commaList . map N.runProperName) dctors ++ ")"
  showRef (P.TypeOpRef op) = "type " ++ N.showOp op
  showRef (P.ValueRef ident) = N.runIdent ident
  showRef (P.ValueOpRef op) = N.showOp op
  showRef (P.TypeClassRef pn) = "class " ++ N.runProperName pn
  showRef (P.TypeInstanceRef ident) = N.runIdent ident
  showRef (P.ModuleRef name) = "module " ++ N.runModuleName name
  showRef (P.PositionedDeclarationRef _ _ ref) = showRef ref

  commaList :: [String] -> String
  commaList = intercalate ", "

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
  => P.Expr
  -> m ()
handleTypeOf val = do
  st <- get
  let m = createTemporaryModule False st val
  e <- liftIO . runMake $ rebuild (map snd (psciLoadedExterns st)) m
  case e of
    Left errs -> printErrors errs
    Right (_, env') ->
      case M.lookup (P.ModuleName [P.ProperName "$PSCI"], P.Ident "it") (P.names env') of
        Just (ty, _, _) -> liftIO . putStrLn . P.prettyPrintType $ ty
        Nothing -> liftIO $ putStrLn "Could not find type"

-- | Takes a type and prints its kind
handleKindOf
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => P.Type
  -> m ()
handleKindOf typ = do
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
            Right (kind, _) -> liftIO . putStrLn . P.prettyPrintKind $ kind
        Nothing -> liftIO $ putStrLn "Could not find kind"

-- | Browse a module and displays its signature
handleBrowse
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => P.ModuleName
  -> m ()
handleBrowse moduleName = do
  st <- get
  env <- asks psciEnvironment
  if isModInEnv moduleName st
    then liftIO . putStrLn $ printModuleSignatures moduleName env
    else case lookupUnQualifiedModName moduleName st of
      Just unQualifiedName ->
        if isModInEnv unQualifiedName st
          then liftIO . putStrLn $ printModuleSignatures unQualifiedName env
          else failNotInEnv moduleName
      Nothing ->
        failNotInEnv moduleName
  where
    isModInEnv modName =
        any ((== modName) . P.getModuleName . fst) . psciLoadedExterns
    failNotInEnv modName =
        liftIO $ putStrLn $ "Module '" ++ N.runModuleName modName ++ "' is not valid."
    lookupUnQualifiedModName quaModName st =
        (\(modName,_,_) -> modName) <$> find ( \(_, _, mayQuaName) -> mayQuaName == Just quaModName) (psciImportedModules st)

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

-- |
-- PureScript Compiler Interactive.
--
module PSCi (runPSCi) where

import Prelude ()
import Prelude.Compat

import Data.Foldable (traverse_)
import Data.List (intercalate, nub, sort, find)
import Data.Tuple (swap)
import qualified Data.Map as M

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT(), runExceptT)
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer.Strict (Writer(), runWriter)

import System.Console.Haskeline
import System.Directory (doesFileExist, getHomeDirectory, getCurrentDirectory)
import System.Exit
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import System.Process (readProcessWithExitCode)
import System.IO.Error (tryIOError)

import qualified Language.PureScript as P
import qualified Language.PureScript.Names as N

import PSCi.Completion (completion)
import PSCi.Parser (parseCommand)
import PSCi.Option
import PSCi.Types
import PSCi.Message
import PSCi.IO
import PSCi.Printer
import PSCi.Module

-- |
-- PSCI monad
--
newtype PSCI a = PSCI { runPSCI :: InputT (StateT PSCiState IO) a } deriving (Functor, Applicative, Monad)

psciIO :: IO a -> PSCI a
psciIO io = PSCI . lift $ lift io

-- |
-- The runner
--
runPSCi :: IO ()
runPSCi = getOpt >>= loop

-- |
-- The PSCI main loop.
--
loop :: PSCiOptions -> IO ()
loop PSCiOptions{..} = do
  config <- loadUserConfig
  inputFiles <- concat <$> traverse glob psciInputFile
  foreignFiles <- concat <$> traverse glob psciForeignInputFiles
  modulesOrFirstError <- loadAllModules inputFiles
  case modulesOrFirstError of
    Left errs -> putStrLn (P.prettyPrintMultipleErrors False errs) >> exitFailure
    Right modules -> do
      historyFilename <- getHistoryFilename
      let settings = defaultSettings { historyFile = Just historyFilename }
      foreignsOrError <- runMake $ do
        foreignFilesContent <- forM foreignFiles (\inFile -> (inFile,) <$> makeIO (const (P.ErrorMessage [] $ P.CannotReadFile inFile)) (readFile inFile))
        P.parseForeignModulesFromFiles foreignFilesContent
      case foreignsOrError of
        Left errs -> putStrLn (P.prettyPrintMultipleErrors False errs) >> exitFailure
        Right foreigns ->
          flip evalStateT (mkPSCiState [] modules foreigns [] psciInputNodeFlags) . runInputT (setComplete completion settings) $ do
            outputStrLn prologueMessage
            traverse_ (traverse_ (runPSCI . handleCommand)) config
            modules' <- lift $ gets psciLoadedModules
            unless (consoleIsDefined (map snd modules')) . outputStrLn $ unlines
              [ "PSCi requires the purescript-console module to be installed."
              , "For help getting started, visit http://wiki.purescript.org/PSCi"
              ]
            go
      where
        go :: InputT (StateT PSCiState IO) ()
        go = do
          c <- getCommand (not psciMultiLineMode)
          case c of
            Left err -> outputStrLn err >> go
            Right Nothing -> go
            Right (Just QuitPSCi) -> outputStrLn quitMessage
            Right (Just c') -> do
              handleInterrupt (outputStrLn "Interrupted.")
                              (withInterrupt (runPSCI (loadAllImportedModules >> handleCommand c')))
              go

-- Compile the module

-- |
-- Load all modules, updating the application state
--
loadAllImportedModules :: PSCI ()
loadAllImportedModules = do
  files <- PSCI . lift $ fmap psciImportedFilenames get
  modulesOrFirstError <- psciIO $ loadAllModules files
  case modulesOrFirstError of
    Left errs -> PSCI $ printErrors errs
    Right modules -> PSCI . lift . modify $ updateModules modules

-- | This is different than the runMake in 'Language.PureScript.Make' in that it specifies the
-- options and ignores the warning messages.
runMake :: P.Make a -> IO (Either P.MultipleErrors a)
runMake mk = fst <$> P.runMake P.defaultOptions mk

makeIO :: (IOError -> P.ErrorMessage) -> IO a -> P.Make a
makeIO f io = do
  e <- liftIO $ tryIOError io
  either (throwError . P.singleError . f) return e

make :: PSCiState -> [P.Module] -> P.Make P.Environment
make st@PSCiState{..} ms = P.make actions' (map snd loadedModules ++ ms)
  where
  filePathMap = M.fromList $ (first P.getModuleName . swap) `map` allModules
  actions = P.buildMakeActions modulesDir filePathMap psciForeignFiles False
  actions' = actions { P.progress = const (return ()) }
  loadedModules = psciLoadedModules st
  allModules = map (first Right) loadedModules ++ map (Left P.RebuildAlways,) ms


-- Commands

-- |
-- Parses the input and returns either a Metacommand, or an error as a string.
--
getCommand :: Bool -> InputT (StateT PSCiState IO) (Either String (Maybe Command))
getCommand singleLineMode = handleInterrupt (return (Right Nothing)) $ do
  firstLine <- withInterrupt $ getInputLine "> "
  case firstLine of
    Nothing -> return (Right (Just QuitPSCi)) -- Ctrl-D when input is empty
    Just "" -> return (Right Nothing)
    Just s | singleLineMode || head s == ':' -> return .fmap Just $ parseCommand s
    Just s -> fmap Just . parseCommand <$> go [s]
  where
    go :: [String] -> InputT (StateT PSCiState IO) String
    go ls = maybe (return . unlines $ reverse ls) (go . (:ls)) =<< getInputLine "  "

-- |
-- Performs an action for each meta-command given, and also for expressions.
--
handleCommand :: Command -> PSCI ()
handleCommand (Expression val) = handleExpression val
handleCommand ShowHelp = PSCI $ outputStrLn helpMessage
handleCommand (Import im) = handleImport im
handleCommand (Decls l) = handleDecls l
handleCommand (LoadFile filePath) = PSCI $ whenFileExists filePath $ \absPath -> do
  m <- lift . lift $ loadModule absPath
  case m of
    Left err -> outputStrLn err
    Right mods -> lift $ modify (updateModules (map (absPath,) mods))
handleCommand (LoadForeign filePath) = PSCI $ whenFileExists filePath $ \absPath -> do
  foreignsOrError <- lift . lift . runMake $ do
    foreignFile <- makeIO (const (P.ErrorMessage [] $ P.CannotReadFile absPath)) (readFile absPath)
    P.parseForeignModulesFromFiles [(absPath, foreignFile)]
  case foreignsOrError of
    Left err -> outputStrLn $ P.prettyPrintMultipleErrors False err
    Right foreigns -> lift $ modify (updateForeignFiles foreigns)
handleCommand ResetState = do
  PSCI . lift . modify $ \st ->
    st { psciImportedModules = []
       , psciLetBindings     = []
       }
  loadAllImportedModules
handleCommand (TypeOf val) = handleTypeOf val
handleCommand (KindOf typ) = handleKindOf typ
handleCommand (BrowseModule moduleName) = handleBrowse moduleName
handleCommand (ShowInfo QueryLoaded) = handleShowLoadedModules
handleCommand (ShowInfo QueryImport) = handleShowImportedModules
handleCommand QuitPSCi = P.internalError "`handleCommand QuitPSCi` was called. This is a bug."


-- |
-- Takes a value expression and evaluates it with the current state.
--
handleExpression :: P.Expr -> PSCI ()
handleExpression val = do
  st <- PSCI $ lift get
  let m = createTemporaryModule True st val
  let nodeArgs = psciNodeFlags st ++ [indexFile]
  e <- psciIO . runMake $ make st [supportModule, m]
  case e of
    Left errs -> PSCI $ printErrors errs
    Right _ -> do
      psciIO $ writeFile indexFile "require('$PSCI')['$main']();"
      process <- psciIO findNodeProcess
      result  <- psciIO $ traverse (\node -> readProcessWithExitCode node nodeArgs "") process
      case result of
        Just (ExitSuccess,   out, _)   -> PSCI $ outputStrLn out
        Just (ExitFailure _, _,   err) -> PSCI $ outputStrLn err
        Nothing                        -> PSCI $ outputStrLn "Couldn't find node.js"

-- |
-- Takes a list of declarations and updates the environment, then run a make. If the declaration fails,
-- restore the original environment.
--
handleDecls :: [P.Declaration] -> PSCI ()
handleDecls ds = do
  st <- PSCI $ lift get
  let st' = updateLets ds st
  let m = createTemporaryModule False st' (P.Literal (P.ObjectLiteral []))
  e <- psciIO . runMake $ make st' [m]
  case e of
    Left err -> PSCI $ printErrors err
    Right _ -> PSCI $ lift (put st')

-- |
-- Show actual loaded modules in psci.
--
handleShowLoadedModules :: PSCI ()
handleShowLoadedModules = do
  loadedModules <- PSCI $ lift $ gets psciLoadedModules
  psciIO $ readModules loadedModules >>= putStrLn
  return ()
  where readModules = return . unlines . sort . nub . map toModuleName
        toModuleName =  N.runModuleName . (\ (P.Module _ _ mdName _ _) -> mdName) . snd

-- |
-- Show the imported modules in psci.
--
handleShowImportedModules :: PSCI ()
handleShowImportedModules = do
  PSCiState { psciImportedModules = importedModules } <- PSCI $ lift get
  psciIO $ showModules importedModules >>= putStrLn
  return ()
  where
  showModules = return . unlines . sort . map showModule
  showModule (mn, declType, asQ) =
    "import " ++ case asQ of
      Just mn' -> "qualified " ++ N.runModuleName mn ++ " as " ++ N.runModuleName mn'
      Nothing  -> N.runModuleName mn ++ " " ++ showDeclType declType

  showDeclType P.Implicit = ""
  showDeclType (P.Explicit refs) = refsList refs
  showDeclType (P.Hiding refs) = " hiding " ++ refsList refs
  refsList refs = " (" ++ commaList (map showRef refs) ++ ")"

  showRef :: P.DeclarationRef -> String
  showRef (P.TypeRef pn dctors) = N.runProperName pn ++ "(" ++ maybe ".." (commaList . map N.runProperName) dctors ++ ")"
  showRef (P.ValueRef ident) = N.runIdent ident
  showRef (P.TypeClassRef pn) = N.runProperName pn
  showRef (P.ProperRef pn) = pn
  showRef (P.TypeInstanceRef ident) = N.runIdent ident
  showRef (P.ModuleRef name) = "module " ++ N.runModuleName name
  showRef (P.PositionedDeclarationRef _ _ ref) = showRef ref

  commaList :: [String] -> String
  commaList = intercalate ", "

-- |
-- Imports a module, preserving the initial state on failure.
--
handleImport :: ImportedModule -> PSCI ()
handleImport im = do
   st <- updateImportedModules im <$> PSCI (lift get)
   let m = createTemporaryModuleForImports st
   e <- psciIO . runMake $ make st [m]
   case e of
     Left errs -> PSCI $ printErrors errs
     Right _  -> do
       PSCI $ lift $ put st
       return ()

-- |
-- Takes a value and prints its type
--
handleTypeOf :: P.Expr -> PSCI ()
handleTypeOf val = do
  st <- PSCI $ lift get
  let m = createTemporaryModule False st val
  e <- psciIO . runMake $ make st [m]
  case e of
    Left errs -> PSCI $ printErrors errs
    Right env' ->
      case M.lookup (P.ModuleName [P.ProperName "$PSCI"], P.Ident "it") (P.names env') of
        Just (ty, _, _) -> PSCI . outputStrLn . P.prettyPrintType $ ty
        Nothing -> PSCI $ outputStrLn "Could not find type"

-- |
-- Browse a module and displays its signature (if module exists).
--
handleBrowse :: P.ModuleName -> PSCI ()
handleBrowse moduleName = do
  st <- PSCI $ lift get
  env <- psciIO . runMake $ make st []
  case env of
    Left errs -> PSCI $ printErrors errs
    Right env' ->
      if isModInEnv moduleName st
        then PSCI $ printModuleSignatures moduleName env'
        else case lookupUnQualifiedModName moduleName st of
          Just unQualifiedName ->
            if isModInEnv unQualifiedName st
              then PSCI $ printModuleSignatures unQualifiedName env'
              else failNotInEnv moduleName
          Nothing ->
            failNotInEnv moduleName
  where
    isModInEnv modName =
        any ((== modName) . P.getModuleName . snd) . psciLoadedModules
    failNotInEnv modName =
        PSCI $ outputStrLn $ "Module '" ++ N.runModuleName modName ++ "' is not valid."
    lookupUnQualifiedModName quaModName st =
        (\(modName,_,_) -> modName) <$> find ( \(_, _, mayQuaName) -> mayQuaName == Just quaModName) (psciImportedModules st)

-- |
-- Takes a value and prints its kind
--
handleKindOf :: P.Type -> PSCI ()
handleKindOf typ = do
  st <- PSCI $ lift get
  let m = createTemporaryModuleForKind st typ
      mName = P.ModuleName [P.ProperName "$PSCI"]
  e <- psciIO . runMake $ make st [m]
  case e of
    Left errs -> PSCI $ printErrors errs
    Right env' ->
      case M.lookup (P.Qualified (Just mName) $ P.ProperName "IT") (P.typeSynonyms env') of
        Just (_, typ') -> do
          let chk = (P.emptyCheckState env') { P.checkCurrentModule = Just mName }
              k   = check (P.kindOf typ') chk

              check :: StateT P.CheckState (ExceptT P.MultipleErrors (Writer P.MultipleErrors)) a -> P.CheckState -> Either P.MultipleErrors (a, P.CheckState)
              check sew cs = fst . runWriter . runExceptT . runStateT sew $ cs
          case k of
            Left errStack   -> PSCI . outputStrLn . P.prettyPrintMultipleErrors False $ errStack
            Right (kind, _) -> PSCI . outputStrLn . P.prettyPrintKind $ kind
        Nothing -> PSCI $ outputStrLn "Could not find kind"

-- Misc

-- |
-- Attempts to read initial commands from '.psci' in the present working
-- directory then the user's home
--
loadUserConfig :: IO (Maybe [Command])
loadUserConfig = onFirstFileMatching readCommands pathGetters
  where
  pathGetters = [getCurrentDirectory, getHomeDirectory]
  readCommands :: IO FilePath -> IO (Maybe [Command])
  readCommands path = do
    configFile <- (</> ".psci") <$> path
    exists <- doesFileExist configFile
    if exists
    then do
      ls <- lines <$> readFile configFile
      case traverse parseCommand ls of
        Left err -> print err >> exitFailure
        Right cs -> return $ Just cs
    else
      return Nothing

-- | Checks if the Console module is defined
consoleIsDefined :: [P.Module] -> Bool
consoleIsDefined = any ((== P.ModuleName (map P.ProperName [ "Control", "Monad", "Eff", "Console" ])) . P.getModuleName)

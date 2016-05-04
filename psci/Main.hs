{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

-- |
-- PureScript Compiler Interactive.
--
module Main (main) where

import Prelude ()
import Prelude.Compat

import Data.Foldable (traverse_)
import Data.List (intercalate, nub, sort, find)
import qualified Data.Map as M

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT(), runExceptT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, runStateT)
import Control.Monad.State.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer.Strict (Writer(), runWriter)

import System.Console.Haskeline
import System.Directory (doesFileExist, getHomeDirectory, getCurrentDirectory)
import System.Exit
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import System.Process (readProcessWithExitCode)
import System.IO.UTF8 (readUTF8File)

import qualified Language.PureScript as P
import qualified Language.PureScript.Externs as P
import qualified Language.PureScript.Names as N

import PSCi.Completion (completion)
import PSCi.Parser (parseCommand)
import PSCi.Option
import PSCi.Types
import PSCi.Message
import PSCi.IO
import PSCi.Printer
import PSCi.Module

-- | This is the top-level application monad which manages input/output
-- and keeps track of the application state.
newtype PSCI a = PSCI { runPSCI :: InputT (StateT PSCiState IO) a } deriving (Functor, Applicative, Monad)

instance MonadIO PSCI where
  liftIO = PSCI . lift . lift

instance MonadState PSCiState PSCI where
  state = PSCI . lift . state

-- | Get command line options and drop into the REPL
main :: IO ()
main = getOpt >>= loop
  where
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
            foreignFilesContent <- forM foreignFiles (\inFile -> (inFile,) <$> readFileMake inFile)
            P.parseForeignModulesFromFiles foreignFilesContent
          case foreignsOrError of
            Left errs -> putStrLn (P.prettyPrintMultipleErrors False errs) >> exitFailure
            Right foreigns -> do
              e <- runMake . make (PSCiState [] [] [] foreigns [] []) . (supportModule :) . map snd $ modules
              case e of
                Left err -> putStrLn (P.prettyPrintMultipleErrors False err) >> exitFailure
                Right (externs, _) ->
                  flip evalStateT (PSCiState [] inputFiles externs foreigns [] psciInputNodeFlags) . runInputT (setComplete completion settings) $ do
                    outputStrLn prologueMessage
                    traverse_ (traverse_ (runPSCI . handleCommand)) config
                    unless (consoleIsDefined externs) . outputStrLn $ unlines
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
                                  (withInterrupt (runPSCI (handleCommand c')))
                  go

-- Compile the module

-- |
-- Load all modules, updating the application state
--
loadAllImportedModules :: PSCI ()
loadAllImportedModules = do
  files <- gets psciLoadedFiles
  modulesOrFirstError <- liftIO $ loadAllModules files
  case modulesOrFirstError of
    Left errs -> PSCI $ printErrors errs
    Right mods -> do
      st <- get
      e <- liftIO . runMake . make st . map snd $ mods
      case e of
        Left err -> PSCI . outputStrLn $ P.prettyPrintMultipleErrors False err
        Right (externs, _) -> modify . updateLoadedExterns $ externs

-- | This is different than the runMake in 'Language.PureScript.Make' in that it specifies the
-- options and ignores the warning messages.
runMake :: P.Make a -> IO (Either P.MultipleErrors a)
runMake mk = fst <$> P.runMake P.defaultOptions mk

readFileMake :: FilePath -> P.Make String
readFileMake path = P.makeIO (const (P.ErrorMessage [] $ P.CannotReadFile path)) (readUTF8File path)

rebuild :: PSCiState -> P.Module -> P.Make P.ExternsFile
rebuild PSCiState{..} m = P.rebuildModule buildActions psciLoadedExterns m
  where
    buildActions :: P.MakeActions P.Make
    buildActions = (P.buildMakeActions modulesDir
                                       filePathMap
                                       psciForeignFiles
                                       False) { P.progress = const (return ()) }

    filePathMap :: M.Map P.ModuleName (Either P.RebuildPolicy FilePath)
    filePathMap = M.singleton (P.getModuleName m) (Left P.RebuildAlways)

make :: PSCiState -> [P.Module] -> P.Make ([P.ExternsFile], P.Environment)
make PSCiState{..} ms = P.make buildActions ms
  where
    buildActions :: P.MakeActions P.Make
    buildActions = (P.buildMakeActions modulesDir
                                       filePathMap
                                       psciForeignFiles
                                       False)

    filePathMap :: M.Map P.ModuleName (Either P.RebuildPolicy FilePath)
    filePathMap = M.fromList $ map (\m -> (P.getModuleName m, Left P.RebuildAlways)) ms

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
handleCommand (LoadFile filePath) = PSCI $ whenFileExists filePath $ \absPath ->
  runPSCI $ do
    e1 <- liftIO $ loadModule absPath
    case e1 of
      Left err -> PSCI $ outputStrLn err
      Right [m] -> do -- todo
        st <- get
        e2 <- liftIO . runMake $ rebuild st m
        case e2 of
          Left err -> PSCI . outputStrLn $ P.prettyPrintMultipleErrors False err
          Right externs -> modify $ updateLoadedExterns [externs] . updateLoadedFiles [absPath]
handleCommand (LoadForeign filePath) = PSCI $ whenFileExists filePath $ \absPath ->
  runPSCI $ do
    foreignsOrError <- liftIO . runMake $ do
      foreignFile <- readFileMake absPath
      P.parseForeignModulesFromFiles [(absPath, foreignFile)]
    case foreignsOrError of
      Left err -> PSCI . outputStrLn $ P.prettyPrintMultipleErrors False err
      Right foreigns -> modify (updateForeignFiles foreigns)
handleCommand ResetState = do
  modify $ \st ->
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
  st <- get
  let m = createTemporaryModule True st val
  let nodeArgs = psciNodeFlags st ++ [indexFile]
  e <- liftIO . runMake $ rebuild st m
  case e of
    Left errs -> PSCI $ printErrors errs
    Right _ -> do
      liftIO $ writeFile indexFile "require('$PSCI')['$main']();"
      process <- liftIO findNodeProcess
      result  <- liftIO $ traverse (\node -> readProcessWithExitCode node nodeArgs "") process
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
  st <- get
  let st' = updateLets ds st
  let m = createTemporaryModule False st' (P.Literal (P.ObjectLiteral []))
  e <- liftIO . runMake $ rebuild st' m
  case e of
    Left err -> PSCI $ printErrors err
    Right _ -> put st'

-- |
-- Show actual loaded modules in psci.
--
handleShowLoadedModules :: PSCI ()
handleShowLoadedModules = do
    loadedModules <- gets psciLoadedExterns
    PSCI $ outputStrLn (readModules loadedModules)
  where
    readModules = unlines . sort . nub . map (P.runModuleName . P.efModuleName)

-- |
-- Show the imported modules in psci.
--
handleShowImportedModules :: PSCI ()
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
  showRef (P.TypeOpRef ident) = "type (" ++ N.runIdent ident ++ ")"
  showRef (P.ValueRef ident) = N.runIdent ident
  showRef (P.TypeClassRef pn) = "class " ++ N.runProperName pn
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
   e <- liftIO . runMake $ rebuild st m
   case e of
     Left errs -> PSCI $ printErrors errs
     Right _  -> put st

-- |
-- Takes a value and prints its type
--
handleTypeOf :: P.Expr -> PSCI ()
handleTypeOf val = do
  st <- get
  let m = createTemporaryModule False st val
  e <- liftIO . runMake $ make st [m] --todo
  case e of
    Left errs -> PSCI $ printErrors errs
    Right (_, env') ->
      case M.lookup (P.ModuleName [P.ProperName "$PSCI"], P.Ident "it") (P.names env') of
        Just (ty, _, _) -> PSCI . outputStrLn . P.prettyPrintType $ ty
        Nothing -> PSCI $ outputStrLn "Could not find type"

-- |
-- Browse a module and displays its signature (if module exists).
--
handleBrowse :: P.ModuleName -> PSCI ()
handleBrowse moduleName = do
  st <- get
  env <- liftIO . runMake $ make st [] -- todo
  case env of
    Left errs -> PSCI $ printErrors errs
    Right (_, env') ->
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
        any ((== modName) . P.efModuleName) . psciLoadedExterns
    failNotInEnv modName =
        PSCI $ outputStrLn $ "Module '" ++ N.runModuleName modName ++ "' is not valid."
    lookupUnQualifiedModName quaModName st =
        (\(modName,_,_) -> modName) <$> find ( \(_, _, mayQuaName) -> mayQuaName == Just quaModName) (psciImportedModules st)

-- |
-- Takes a value and prints its kind
--
handleKindOf :: P.Type -> PSCI ()
handleKindOf typ = do
  st <- get
  let m = createTemporaryModuleForKind st typ
      mName = P.ModuleName [P.ProperName "$PSCI"]
  e <- liftIO . runMake $ make st [m] -- todo
  case e of
    Left errs -> PSCI $ printErrors errs
    Right (_, env') ->
      case M.lookup (P.Qualified (Just mName) $ P.ProperName "IT") (P.typeSynonyms env') of
        Just (_, typ') -> do
          let chk = (P.emptyCheckState env') { P.checkCurrentModule = Just mName }
              k   = check (P.kindOf typ') chk

              check :: StateT P.CheckState (ExceptT P.MultipleErrors (Writer P.MultipleErrors)) a -> P.CheckState -> Either P.MultipleErrors (a, P.CheckState)
              check sew = fst . runWriter . runExceptT . runStateT sew
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
      ls <- lines <$> readUTF8File configFile
      case traverse parseCommand ls of
        Left err -> print err >> exitFailure
        Right cs -> return $ Just cs
    else
      return Nothing

-- | Checks if the Console module is defined
consoleIsDefined :: [P.ExternsFile] -> Bool
consoleIsDefined = any ((== P.ModuleName (map P.ProperName [ "Control", "Monad", "Eff", "Console" ])) . P.efModuleName)

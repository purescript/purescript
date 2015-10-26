-----------------------------------------------------------------------------
--
-- Module      :  PSCi
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- PureScript Compiler Interactive.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module PSCi where

import Data.Foldable (traverse_)
import Data.List (intercalate, nub, sort)
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable (traverse)
#endif
import Data.Tuple (swap)
import Data.Version (showVersion)
import qualified Data.Map as M

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer.Strict (runWriter)
import qualified Control.Monad.Trans.State.Lazy as L

import Options.Applicative as Opts

import System.Console.Haskeline
import System.Directory (doesFileExist, findExecutable, getHomeDirectory, getCurrentDirectory)
import System.Exit
import System.FilePath (pathSeparator, (</>), isPathSeparator)
import System.FilePath.Glob (glob)
import System.Process (readProcessWithExitCode)
import System.IO.Error (tryIOError)

import qualified Language.PureScript as P
import qualified Language.PureScript.Names as N
import qualified Paths_purescript as Paths

import qualified Directive as D
import Completion (completion)
import IO (mkdirp)
import Parser (parseCommand)
import Types

-- | The name of the PSCI support module
supportModuleName :: P.ModuleName
supportModuleName = P.ModuleName [P.ProperName "$PSCI", P.ProperName "Support"]

-- | Support module, contains code to evaluate terms
supportModule :: P.Module
supportModule =
  case P.parseModulesFromFiles id [("", code)] of
    Right [(_, P.Module ss cs _ ds exps)] -> P.Module ss cs supportModuleName ds exps
    _ -> P.internalError "Support module could not be parsed"
  where
  code :: String
  code = unlines
    [ "module S where"
    , ""
    , "import Prelude"
    , "import Control.Monad.Eff"
    , "import Control.Monad.Eff.Console"
    , "import Control.Monad.Eff.Unsafe"
    , ""
    , "class Eval a where"
    , "  eval :: a -> Eff (console :: CONSOLE) Unit"
    , ""
    , "instance evalShow :: (Show a) => Eval a where"
    , "  eval = print"
    , ""
    , "instance evalEff :: (Eval a) => Eval (Eff eff a) where"
    , "  eval x = unsafeInterleaveEff x >>= eval"
    ]

-- File helpers

onFirstFileMatching :: Monad m => (b -> m (Maybe a)) -> [b] -> m (Maybe a)
onFirstFileMatching f pathVariants = runMaybeT . msum $ map (MaybeT . f) pathVariants

-- |
-- Locates the node executable.
-- Checks for either @nodejs@ or @node@.
--
findNodeProcess :: IO (Maybe String)
findNodeProcess = onFirstFileMatching findExecutable names
  where names = ["nodejs", "node"]

-- |
-- Grabs the filename where the history is stored.
--
getHistoryFilename :: IO FilePath
getHistoryFilename = do
  home <- getHomeDirectory
  let filename = home </> ".purescript" </> "psci_history"
  mkdirp filename
  return filename

-- |
-- Loads a file for use with imports.
--
loadModule :: FilePath -> IO (Either String [P.Module])
loadModule filename = do
  content <- readFile filename
  return $ either (Left . P.prettyPrintMultipleErrors False) (Right . map snd) $ P.parseModulesFromFiles id [(filename, content)]

-- |
-- Load all modules.
--
loadAllModules :: [FilePath] -> IO (Either P.MultipleErrors [(Either P.RebuildPolicy FilePath, P.Module)])
loadAllModules files = do
  filesAndContent <- forM files $ \filename -> do
    content <- readFile filename
    return (Right filename, content)
  return $ P.parseModulesFromFiles (either (const "") id) filesAndContent

-- |
-- Load all modules, updating the application state
--
loadAllImportedModules :: PSCI ()
loadAllImportedModules = do
  files <- PSCI . lift $ fmap psciImportedFilenames get
  modulesOrFirstError <- psciIO $ loadAllModules files
  case modulesOrFirstError of
    Left errs -> printErrors errs
    Right modules -> PSCI . lift . modify $ \st -> st { psciLoadedModules = modules }

-- |
-- Expands tilde in path.
--
expandTilde :: FilePath -> IO FilePath
expandTilde ('~':p:rest) | isPathSeparator p = (</> rest) <$> getHomeDirectory
expandTilde p = return p

-- Messages

-- |
-- The help message.
--
helpMessage :: String
helpMessage = "The following commands are available:\n\n    " ++
  intercalate "\n    " (map line D.help) ++
  "\n\n" ++ extraHelp
  where
  line :: (Directive, String, String) -> String
  line (dir, arg, desc) =
    let cmd = ':' : D.stringFor dir
    in unwords [ cmd
               , replicate (11 - length cmd) ' '
               , arg
               , replicate (11 - length arg) ' '
               , desc
               ]

  extraHelp =
    "Further information is available on the PureScript wiki:\n" ++
    " --> https://github.com/purescript/purescript/wiki/psci"


-- |
-- The welcome prologue.
--
prologueMessage :: String
prologueMessage = intercalate "\n"
  [ " ____                 ____            _       _   "
  , "|  _ \\ _   _ _ __ ___/ ___|  ___ _ __(_)_ __ | |_ "
  , "| |_) | | | | '__/ _ \\___ \\ / __| '__| | '_ \\| __|"
  , "|  __/| |_| | | |  __/___) | (__| |  | | |_) | |_ "
  , "|_|    \\__,_|_|  \\___|____/ \\___|_|  |_| .__/ \\__|"
  , "                                       |_|        "
  , ""
  , ":? shows help"
  ]

-- |
-- The quit message.
--
quitMessage :: String
quitMessage = "See ya!"

-- |
-- PSCI monad
--
newtype PSCI a = PSCI { runPSCI :: InputT (StateT PSCiState IO) a } deriving (Functor, Applicative, Monad)

psciIO :: IO a -> PSCI a
psciIO io = PSCI . lift $ lift io

-- |
-- Makes a volatile module to execute the current expression.
--
createTemporaryModule :: Bool -> PSCiState -> P.Expr -> P.Module
createTemporaryModule exec PSCiState{psciImportedModules = imports, psciLetBindings = lets} val =
  let
    moduleName = P.ModuleName [P.ProperName "$PSCI"]
    trace = P.Var (P.Qualified (Just supportModuleName) (P.Ident "eval"))
    mainValue = P.App trace (P.Var (P.Qualified Nothing (P.Ident "it")))
    itDecl = P.ValueDeclaration (P.Ident "it") P.Public [] $ Right val
    mainDecl = P.ValueDeclaration (P.Ident "main") P.Public [] $ Right mainValue
    decls = if exec then [itDecl, mainDecl] else [itDecl]
  in
    P.Module (P.internalModuleSourceSpan "<internal>") [] moduleName ((importDecl `map` imports) ++ lets ++ decls) Nothing


-- |
-- Makes a volatile module to hold a non-qualified type synonym for a fully-qualified data type declaration.
--
createTemporaryModuleForKind :: PSCiState -> P.Type -> P.Module
createTemporaryModuleForKind PSCiState{psciImportedModules = imports, psciLetBindings = lets} typ =
  let
    moduleName = P.ModuleName [P.ProperName "$PSCI"]
    itDecl = P.TypeSynonymDeclaration (P.ProperName "IT") [] typ
  in
    P.Module (P.internalModuleSourceSpan "<internal>") [] moduleName ((importDecl `map` imports) ++ lets ++ [itDecl]) Nothing

-- |
-- Makes a volatile module to execute the current imports.
--
createTemporaryModuleForImports :: PSCiState -> P.Module
createTemporaryModuleForImports PSCiState{psciImportedModules = imports} =
  let
    moduleName = P.ModuleName [P.ProperName "$PSCI"]
  in
    P.Module (P.internalModuleSourceSpan "<internal>") [] moduleName (importDecl `map` imports) Nothing

importDecl :: ImportedModule -> P.Declaration
importDecl (mn, declType, asQ) = P.ImportDeclaration mn declType asQ

indexFile :: FilePath
indexFile = ".psci_modules" ++ pathSeparator : "index.js"

modulesDir :: FilePath
modulesDir = ".psci_modules" ++ pathSeparator : "node_modules"

-- | This is different than the runMake in 'Language.PureScript.Make' in that it specifies the
-- options and ignores the warning messages.
runMake :: P.Make a -> IO (Either P.MultipleErrors a)
runMake mk = fmap fst $ P.runMake P.defaultOptions mk

makeIO :: (IOError -> P.ErrorMessage) -> IO a -> P.Make a
makeIO f io = do
  e <- liftIO $ tryIOError io
  either (throwError . P.singleError . f) return e

make :: PSCiState -> [(Either P.RebuildPolicy FilePath, P.Module)] -> P.Make P.Environment
make PSCiState{..} ms = P.make actions' (map snd (psciLoadedModules ++ ms))
  where
  filePathMap = M.fromList $ (first P.getModuleName . swap) `map` (psciLoadedModules ++ ms)
  actions = P.buildMakeActions modulesDir filePathMap psciForeignFiles False
  actions' = actions { P.progress = const (return ()) }

-- |
-- Takes a value declaration and evaluates it with the current state.
--
handleDeclaration :: P.Expr -> PSCI ()
handleDeclaration val = do
  st <- PSCI $ lift get
  let m = createTemporaryModule True st val
  let nodeArgs = psciNodeFlags st ++ [indexFile]
  e <- psciIO . runMake $ make st [(Left P.RebuildAlways, supportModule), (Left P.RebuildAlways, m)]
  case e of
    Left errs -> printErrors errs
    Right _ -> do
      psciIO $ writeFile indexFile "require('$PSCI').main();"
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
  let m = createTemporaryModule False st' (P.ObjectLiteral [])
  e <- psciIO . runMake $ make st' [(Left P.RebuildAlways, m)]
  case e of
    Left err -> printErrors err
    Right _ -> PSCI $ lift (put st')

-- |
-- Show actual loaded modules in psci.
--
handleShowLoadedModules :: PSCI ()
handleShowLoadedModules = do
  PSCiState { psciLoadedModules = loadedModules } <- PSCI $ lift get
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
  showDeclType (P.Hiding refs) = "hiding " ++ refsList refs
  refsList refs = "(" ++ commaList (map showRef refs) ++ ")"

  showRef :: P.DeclarationRef -> String
  showRef (P.TypeRef pn dctors) = show pn ++ "(" ++ maybe ".." (commaList . map N.runProperName) dctors ++ ")"
  showRef (P.ValueRef ident) = show ident
  showRef (P.TypeClassRef pn) = show pn
  showRef (P.TypeInstanceRef ident) = show ident
  showRef (P.ModuleRef name) = "module " ++ show name
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
   e <- psciIO . runMake $ make st [(Left P.RebuildAlways, m)]
   case e of
     Left errs -> printErrors errs
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
  e <- psciIO . runMake $ make st [(Left P.RebuildAlways, m)]
  case e of
    Left errs -> printErrors errs
    Right env' ->
      case M.lookup (P.ModuleName [P.ProperName "$PSCI"], P.Ident "it") (P.names env') of
        Just (ty, _, _) -> PSCI . outputStrLn . P.prettyPrintType $ ty
        Nothing -> PSCI $ outputStrLn "Could not find type"

-- |
-- Pretty print a module's signatures
--
printModuleSignatures :: P.ModuleName -> P.Environment -> PSCI ()
printModuleSignatures moduleName env =
  PSCI $ let namesEnv = P.names env
             moduleNamesIdent = (filter ((== moduleName) . fst) . M.keys) namesEnv
             in case moduleNamesIdent of
                  [] -> outputStrLn $ "This module '"++ P.runModuleName moduleName ++"' does not export functions."
                  _ -> ( outputStrLn
                       . unlines
                       . sort
                       . map (showType . findType namesEnv)) moduleNamesIdent
  where findType :: M.Map (P.ModuleName, P.Ident) (P.Type, P.NameKind, P.NameVisibility) -> (P.ModuleName, P.Ident) -> (P.Ident, Maybe (P.Type, P.NameKind, P.NameVisibility))
        findType envNames m@(_, mIdent) = (mIdent, M.lookup m envNames)
        showType :: (P.Ident, Maybe (P.Type, P.NameKind, P.NameVisibility)) -> String
        showType (mIdent, Just (mType, _, _)) = show mIdent ++ " :: " ++ P.prettyPrintType mType
        showType _ = P.internalError "The impossible happened in printModuleSignatures."

-- |
-- Browse a module and displays its signature (if module exists).
--
handleBrowse :: P.ModuleName -> PSCI ()
handleBrowse moduleName = do
  st <- PSCI $ lift get
  env <- psciIO . runMake $ make st []
  case env of
    Left errs -> printErrors errs
    Right env' ->
      if moduleName `notElem` (nub . map ((\ (P.Module _ _ modName _ _ ) -> modName) . snd)) (psciLoadedModules st)
        then PSCI $ outputStrLn $ "Module '" ++ N.runModuleName moduleName ++ "' is not valid."
        else printModuleSignatures moduleName env'

-- | Pretty-print errors
printErrors :: P.MultipleErrors -> PSCI ()
printErrors = PSCI . outputStrLn . P.prettyPrintMultipleErrors False

-- |
-- Takes a value and prints its kind
--
handleKindOf :: P.Type -> PSCI ()
handleKindOf typ = do
  st <- PSCI $ lift get
  let m = createTemporaryModuleForKind st typ
      mName = P.ModuleName [P.ProperName "$PSCI"]
  e <- psciIO . runMake $ make st [(Left P.RebuildAlways, m)]
  case e of
    Left errs -> printErrors errs
    Right env' ->
      case M.lookup (P.Qualified (Just mName) $ P.ProperName "IT") (P.typeSynonyms env') of
        Just (_, typ') -> do
          let chk = P.CheckState env' 0 0 (Just mName)
              k   = fst . runWriter . runExceptT $ L.runStateT (P.unCheck (P.kindOf typ')) chk
          case k of
            Left errStack   -> PSCI . outputStrLn . P.prettyPrintMultipleErrors False $ errStack
            Right (kind, _) -> PSCI . outputStrLn . P.prettyPrintKind $ kind
        Nothing -> PSCI $ outputStrLn "Could not find kind"

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
    Just s | singleLineMode || head s == ':' -> return . either Left (Right . Just) $ parseCommand s
    Just s -> either Left (Right . Just) . parseCommand <$> go [s]
  where
    go :: [String] -> InputT (StateT PSCiState IO) String
    go ls = maybe (return . unlines $ reverse ls) (go . (:ls)) =<< getInputLine "  "

-- |
-- Performs an action for each meta-command given, and also for expressions.
--
handleCommand :: Command -> PSCI ()
handleCommand (Expression val) = handleDeclaration val
handleCommand ShowHelp = PSCI $ outputStrLn helpMessage
handleCommand (Import im) = handleImport im
handleCommand (Decls l) = handleDecls l
handleCommand (LoadFile filePath) = whenFileExists filePath $ \absPath -> do
  PSCI . lift $ modify (updateImportedFiles absPath)
  m <- psciIO $ loadModule absPath
  case m of
    Left err -> PSCI $ outputStrLn err
    Right mods -> PSCI . lift $ modify (updateModules (map ((,) (Right absPath)) mods))
handleCommand (LoadForeign filePath) = whenFileExists filePath $ \absPath -> do
  foreignsOrError <- psciIO . runMake $ do
    foreignFile <- makeIO (const (P.ErrorMessage [] $ P.CannotReadFile absPath)) (readFile absPath)
    P.parseForeignModulesFromFiles [(absPath, foreignFile)]
  case foreignsOrError of
    Left err -> PSCI $ outputStrLn $ P.prettyPrintMultipleErrors False err
    Right foreigns -> PSCI . lift $ modify (updateForeignFiles foreigns)
handleCommand ResetState = do
  files <- psciImportedFilenames <$> PSCI (lift get)
  PSCI . lift . modify $ \st -> st
    { psciImportedFilenames   = files
    , psciImportedModules     = []
    , psciLetBindings         = []
    }
  loadAllImportedModules
handleCommand (TypeOf val) = handleTypeOf val
handleCommand (KindOf typ) = handleKindOf typ
handleCommand (BrowseModule moduleName) = handleBrowse moduleName
handleCommand (ShowInfo QueryLoaded) = handleShowLoadedModules
handleCommand (ShowInfo QueryImport) = handleShowImportedModules
handleCommand QuitPSCi = P.internalError "`handleCommand QuitPSCi` was called. This is a bug."

whenFileExists :: FilePath -> (FilePath -> PSCI ()) -> PSCI ()
whenFileExists filePath f = do
  absPath <- psciIO $ expandTilde filePath
  exists <- psciIO $ doesFileExist absPath
  if exists
    then f absPath
    else PSCI . outputStrLn $ "Couldn't locate: " ++ filePath

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
      case mapM parseCommand ls of
        Left err -> print err >> exitFailure
        Right cs -> return $ Just cs
    else
      return Nothing


-- | Checks if the Console module is defined
consoleIsDefined :: [P.Module] -> Bool
consoleIsDefined = any ((== P.ModuleName (map P.ProperName [ "Control", "Monad", "Eff", "Console" ])) . P.getModuleName)

-- |
-- The PSCI main loop.
--
loop :: PSCiOptions -> IO ()
loop PSCiOptions{..} = do
  config <- loadUserConfig
  inputFiles <- concat <$> mapM glob psciInputFile
  foreignFiles <- concat <$> mapM glob psciForeignInputFiles
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
          flip evalStateT (PSCiState inputFiles [] modules foreigns [] psciInputNodeFlags) . runInputT (setComplete completion settings) $ do
            outputStrLn prologueMessage
            traverse_ (mapM_ (runPSCI . handleCommand)) config
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

multiLineMode :: Parser Bool
multiLineMode = switch $
     long "multi-line-mode"
  <> short 'm'
  <> Opts.help "Run in multi-line mode (use ^D to terminate commands)"

inputFile :: Parser FilePath
inputFile = strArgument $
     metavar "FILE"
  <> Opts.help "Optional .purs files to load on start"

inputForeignFile :: Parser FilePath
inputForeignFile = strOption $
     short 'f'
  <> long "ffi"
  <> help "The input .js file(s) providing foreign import implementations"

nodeFlagsFlag :: Parser [String]
nodeFlagsFlag = option parser $
     long "node-opts"
  <> metavar "NODE_OPTS"
  <> value []
  <> Opts.help "Flags to pass to node, separated by spaces"
  where
    parser = words <$> str

psciOptions :: Parser PSCiOptions
psciOptions = PSCiOptions <$> multiLineMode
                          <*> many inputFile
                          <*> many inputForeignFile
                          <*> nodeFlagsFlag

runPSCi :: IO ()
runPSCi = execParser opts >>= loop
  where
  opts        = info (version <*> helper <*> psciOptions) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "psci - Interactive mode for PureScript"
  footerInfo  = footer $ "psci " ++ showVersion Paths.version

  version :: Parser (a -> a)
  version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> Opts.help "Show the version number" <> hidden

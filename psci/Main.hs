-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- PureScript Compiler Interactive.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Foldable (traverse_)
import Data.List (intercalate, isPrefixOf, nub, sortBy, sort)
import Data.Maybe (mapMaybe)
import Data.Traversable (traverse)
import Data.Version (showVersion)
import Data.Char (isSpace)
import qualified Data.Map as M

import Control.Applicative
import Control.Monad
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.State.Strict
import qualified Control.Monad.Trans.State.Lazy as L

import Options.Applicative as Opts

import System.Console.Haskeline
import System.Directory (createDirectoryIfMissing, getModificationTime, doesFileExist, findExecutable, getHomeDirectory, getCurrentDirectory)
import System.Exit
import System.FilePath (pathSeparator, takeDirectory, (</>), isPathSeparator)
import System.IO.Error (tryIOError)
import System.Process (readProcessWithExitCode)

import qualified Text.Parsec as Par (ParseError)

import qualified Language.PureScript as P
import qualified Language.PureScript.AST as D
import qualified Language.PureScript.Names as N
import qualified Paths_purescript as Paths

import qualified Commands as C
import qualified Directive as D
import Parser

data PSCiOptions = PSCiOptions
  { psciMultiLineMode  :: Bool
  , psciInputFile      :: [FilePath]
  , psciInputNodeFlags :: [String]
  }

-- |
-- The PSCI state.
-- Holds a list of imported modules, loaded files, and partial let bindings.
-- The let bindings are partial,
-- because it makes more sense to apply the binding to the final evaluated expression.
--
data PSCiState = PSCiState
  { psciImportedFilenames   :: [FilePath]
  , psciImportedModules     :: [C.ImportedModule]
  , psciLoadedModules       :: [(Either P.RebuildPolicy FilePath, P.Module)]
  , psciLetBindings         :: [P.Declaration]
  , psciNodeFlags           :: [String]
  }

psciImportedModuleNames :: PSCiState -> [P.ModuleName]
psciImportedModuleNames (PSCiState{psciImportedModules = is}) =
  map (\(mn, _, _) -> mn) is

-- State helpers

-- |
-- Updates the state to have more imported modules.
--
updateImportedFiles :: FilePath -> PSCiState -> PSCiState
updateImportedFiles filename st = st { psciImportedFilenames = filename : psciImportedFilenames st }

-- |
-- Updates the state to have more imported modules.
--
updateImportedModules :: C.ImportedModule -> PSCiState -> PSCiState
updateImportedModules im st = st { psciImportedModules = im : psciImportedModules st }

-- |
-- Updates the state to have more loaded files.
--
updateModules :: [(Either P.RebuildPolicy FilePath, P.Module)] -> PSCiState -> PSCiState
updateModules modules st = st { psciLoadedModules = psciLoadedModules st ++ modules }

-- |
-- Updates the state to have more let bindings.
--
updateLets :: [P.Declaration] -> PSCiState -> PSCiState
updateLets ds st = st { psciLetBindings = psciLetBindings st ++ ds }

-- File helpers
-- |
-- Load the necessary modules.
--
defaultImports :: [C.ImportedModule]
defaultImports = [(P.ModuleName [P.ProperName "Prelude"], D.Implicit, Nothing)]

-- |
-- Locates the node executable.
-- Checks for either @nodejs@ or @node@.
--
findNodeProcess :: IO (Maybe String)
findNodeProcess = runMaybeT . msum $ map (MaybeT . findExecutable) names
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
  return $ either (Left . show) (Right . map snd) $ P.parseModulesFromFiles id [(filename, content)]

-- |
-- Load all modules, including the Prelude
--
loadAllModules :: [FilePath] -> IO (Either Par.ParseError [(Either P.RebuildPolicy FilePath, P.Module)])
loadAllModules files = do
  filesAndContent <- forM files $ \filename -> do
    content <- readFile filename
    return (Right filename, content)
  return $ P.parseModulesFromFiles (either (const "") id) $ (Left P.RebuildNever, P.prelude) : filesAndContent

-- |
-- Load all modules, updating the application state
--
loadAllImportedModules :: PSCI ()
loadAllImportedModules = do
  files <- PSCI . lift $ fmap psciImportedFilenames get
  modulesOrFirstError <- psciIO $ loadAllModules files
  case modulesOrFirstError of
    Left err -> psciIO $ print err
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
  intercalate "\n    " (map line D.help)
  where
    line :: (D.Directive, String, String) -> String
    line (dir, arg, desc) = intercalate " "
          [ cmd
          , replicate (11 - length cmd) ' '
          , arg
          , replicate (11 - length arg) ' '
          , desc
          ]
      where cmd = ":" ++ head (D.commands dir)

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

-- Haskeline completions

data CompletionContext = Command String | FilePath String | Module | Identifier
                       | Type | Fixed [String] | Multiple [CompletionContext]
                         deriving (Show)

-- |
-- Decide what kind of completion we need based on input.
completionContext :: String -> String -> Maybe CompletionContext
completionContext cmd@"" _ = Just $ Multiple [Command cmd, Identifier]
completionContext (':' : cmd) word =
  case D.parseDirective dstr of
    Just directive | dstr `elem` D.commands directive -> context directive
    _ -> Just $ Command cmd
  where
  dstr :: String
  dstr = takeWhile (not . isSpace) cmd

  context :: D.Directive -> Maybe CompletionContext
  context D.Browse = Just Module
  context D.Load = Just $ FilePath word
  context D.Quit = Nothing
  context D.Reset = Nothing
  context D.Help = Nothing
  context D.Show = Just $ Fixed ["import", "loaded"]
  context D.Type = Just Identifier
  context D.Kind = Just Type
completionContext _ _ = Just Identifier

-- |
-- Loads module, function, and file completions.
--
completion :: CompletionFunc (StateT PSCiState IO)
completion = completeWordWithPrev Nothing " \t\n\r" findCompletions
  where
  findCompletions :: String -> String -> StateT PSCiState IO [Completion]
  findCompletions prev word = do
    let ctx = completionContext ((dropWhile isSpace (reverse prev)) ++ word) word
    completions <- case ctx of
      Nothing -> return []
      (Just c) -> (mapMaybe $ either (\cand -> if word `isPrefixOf` cand
                                               then Just $ simpleCompletion cand
                                               else Nothing) Just)
                  <$> getCompletion c
    return $ sortBy sorter completions

  getCompletion :: CompletionContext -> StateT PSCiState IO [Either String Completion]
  getCompletion (FilePath f) = (map Right) <$> listFiles f
  getCompletion Module = (map Left) <$> getModuleNames
  getCompletion Identifier = (map Left) <$> ((++) <$> getIdentNames <*> getDctorNames)
  getCompletion Type = (map Left) <$> getTypeNames
  getCompletion (Fixed list) = return $ (map Left) list
  getCompletion (Multiple contexts) = concat <$> mapM getCompletion contexts
  getCompletion (Command cmd) = return . map (Left . (":" ++)) . nub $ matching
    where
    matching :: [String]
    matching = filter (isPrefixOf cmd) . concatMap (D.commands) $ D.directives

  getLoadedModules :: StateT PSCiState IO [P.Module]
  getLoadedModules = map snd . psciLoadedModules <$> get

  getModuleNames :: StateT PSCiState IO [String]
  getModuleNames = moduleNames <$> getLoadedModules

  mapLoadedModulesAndQualify :: (Show a) => (P.Module -> [a]) -> StateT PSCiState IO [String]
  mapLoadedModulesAndQualify f = do
    ms <- getLoadedModules
    q <- sequence [qualifyIfNeeded m (f m) | m <- ms]
    return $ concat q

  getIdentNames :: StateT PSCiState IO [String]
  getIdentNames = mapLoadedModulesAndQualify identNames

  getDctorNames :: StateT PSCiState IO [String]
  getDctorNames = mapLoadedModulesAndQualify dctorNames

  getTypeNames :: StateT PSCiState IO [String]
  getTypeNames = mapLoadedModulesAndQualify typeDecls

  qualifyIfNeeded :: (Show a) => P.Module -> [a] -> StateT PSCiState IO [String]
  qualifyIfNeeded m decls = do
    let name = P.getModuleName m
    imported <- psciImportedModuleNames <$> get
    let qualified = map (P.Qualified $ Just name) decls
    if name `elem` imported then
        return $ map show $ qualified ++ (map (P.Qualified Nothing) decls)
    else
        return $ map show qualified

  typeDecls :: P.Module -> [N.ProperName]
  typeDecls m = mapMaybe getTypeName $ filter P.isDataDecl (P.exportedDeclarations m)
      where getTypeName :: P.Declaration -> Maybe N.ProperName
            getTypeName (P.TypeSynonymDeclaration name _ _) = Just name
            getTypeName (P.DataDeclaration _ name _ _) = Just name
            getTypeName (P.PositionedDeclaration _ _ d) = getTypeName d
            getTypeName _ = Nothing

  identNames :: P.Module -> [N.Ident]
  identNames (P.Module _ _ ds exports) = nub [ ident | ident <- mapMaybe (getDeclName exports) (D.flattenDecls ds) ]
    where getDeclName :: Maybe [P.DeclarationRef] -> P.Declaration -> Maybe P.Ident
          getDeclName exts decl@(P.ValueDeclaration ident _ _ _)  | P.isExported exts decl = Just ident
          getDeclName exts decl@(P.ExternDeclaration _ ident _ _) | P.isExported exts decl = Just ident
          getDeclName exts (P.PositionedDeclaration _ _ d) = getDeclName exts d
          getDeclName _ _ = Nothing

  dctorNames :: P.Module -> [N.ProperName]
  dctorNames m = nub $ concat $ map (P.exportedDctors m) dnames
    where getDataDeclName :: P.Declaration -> Maybe N.ProperName
          getDataDeclName (P.DataDeclaration _ name _ _) = Just name
          getDataDeclName (P.PositionedDeclaration _ _ d) = getDataDeclName d
          getDataDeclName _ = Nothing

          dnames :: [N.ProperName]
          dnames = (mapMaybe getDataDeclName onlyDataDecls)

          onlyDataDecls :: [P.Declaration]
          onlyDataDecls = (filter P.isDataDecl (P.exportedDeclarations m))

  moduleNames :: [P.Module] -> [String]
  moduleNames ms = nub [show moduleName | P.Module _ moduleName _ _ <- ms]

  sorter :: Completion -> Completion -> Ordering
  sorter (Completion _ d1 _) (Completion _ d2 _) = if ":" `isPrefixOf` d1 then LT else compare d1 d2

-- Compilation

-- | Compilation options.
--
options :: P.Options P.Make
options = P.defaultMakeOptions

-- |
-- PSCI monad
--
newtype PSCI a = PSCI { runPSCI :: InputT (StateT PSCiState IO) a } deriving (Functor, Applicative, Monad)

psciIO :: IO a -> PSCI a
psciIO io = PSCI . lift $ lift io

newtype Make a = Make { unMake :: ReaderT (P.Options P.Make) (ExceptT String IO) a }
  deriving (Functor, Applicative, Monad, MonadError String, MonadReader (P.Options P.Make))

runMake :: Make a -> IO (Either String a)
runMake = runExceptT . flip runReaderT options . unMake

makeIO :: IO a -> Make a
makeIO = Make . lift . ExceptT . fmap (either (Left . show) Right) . tryIOError

instance P.MonadMake Make where
  getTimestamp path = makeIO $ do
    exists <- doesFileExist path
    traverse (const $ getModificationTime path) $ guard exists
  readTextFile path = makeIO $ readFile path
  writeTextFile path text = makeIO $ do
    mkdirp path
    writeFile path text
  progress s = unless (s == "Compiling $PSCI") $ makeIO . putStrLn $ s

mkdirp :: FilePath -> IO ()
mkdirp = createDirectoryIfMissing True . takeDirectory

-- |
-- Makes a volatile module to execute the current expression.
--
createTemporaryModule :: Bool -> PSCiState -> P.Expr -> P.Module
createTemporaryModule exec PSCiState{psciImportedModules = imports, psciLetBindings = lets} val =
  let
    moduleName = P.ModuleName [P.ProperName "$PSCI"]
    traceModule = P.ModuleName [P.ProperName "Debug", P.ProperName "Trace"]
    trace = P.Var (P.Qualified (Just traceModule) (P.Ident "print"))
    mainValue = P.App trace (P.Var (P.Qualified Nothing (P.Ident "it")))
    itDecl = P.ValueDeclaration (P.Ident "it") P.Value [] $ Right val
    mainDecl = P.ValueDeclaration (P.Ident "main") P.Value [] $ Right mainValue
    decls = if exec then [itDecl, mainDecl] else [itDecl]
  in
    P.Module [] moduleName ((importDecl `map` imports) ++ lets ++ decls) Nothing


-- |
-- Makes a volatile module to hold a non-qualified type synonym for a fully-qualified data type declaration.
--
createTemporaryModuleForKind :: PSCiState -> P.Type -> P.Module
createTemporaryModuleForKind PSCiState{psciImportedModules = imports} typ =
  let
    moduleName = P.ModuleName [P.ProperName "$PSCI"]
    itDecl = P.TypeSynonymDeclaration (P.ProperName "IT") [] typ
  in
    P.Module [] moduleName ((importDecl `map` imports) ++ [itDecl]) Nothing

-- |
-- Makes a volatile module to execute the current imports.
--
createTemporaryModuleForImports :: PSCiState -> P.Module
createTemporaryModuleForImports PSCiState{psciImportedModules = imports} =
  let
    moduleName = P.ModuleName [P.ProperName "$PSCI"]
  in
    P.Module [] moduleName (importDecl `map` imports) Nothing

importDecl :: C.ImportedModule -> P.Declaration
importDecl (mn, declType, asQ) = P.ImportDeclaration mn declType asQ

modulesDir :: FilePath
modulesDir = ".psci_modules" ++ pathSeparator : "node_modules"

indexFile :: FilePath
indexFile = ".psci_modules" ++ pathSeparator : "index.js"

-- |
-- Takes a value declaration and evaluates it with the current state.
--
handleDeclaration :: P.Expr -> PSCI ()
handleDeclaration val = do
  st <- PSCI $ lift get
  let m = createTemporaryModule True st val
  let nodeArgs = psciNodeFlags st ++ [indexFile]
  e <- psciIO . runMake $ P.make modulesDir (psciLoadedModules st ++ [(Left P.RebuildAlways, m)]) []
  case e of
    Left err -> PSCI $ outputStrLn err
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
  e <- psciIO . runMake $ P.make modulesDir (psciLoadedModules st' ++ [(Left P.RebuildAlways, m)]) []
  case e of
    Left err -> PSCI $ outputStrLn err
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
        toModuleName =  N.runModuleName . (\ (D.Module _ mdName _ _) -> mdName) . snd

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

  showDeclType D.Implicit = ""
  showDeclType (D.Explicit refs) = refsList refs
  showDeclType (D.Hiding refs) = "hiding " ++ refsList refs
  refsList refs = "(" ++ commaList (map showRef refs) ++ ")"

  showRef :: P.DeclarationRef -> String
  showRef (D.TypeRef pn dctors) = show pn ++ "(" ++ maybe ".." (commaList . map N.runProperName) dctors ++ ")"
  showRef (D.ValueRef ident) = show ident
  showRef (D.TypeClassRef pn) = show pn
  showRef (D.TypeInstanceRef ident) = show ident
  showRef (D.PositionedDeclarationRef _ _ ref) = showRef ref

  commaList :: [String] -> String
  commaList = intercalate ", "

-- |
-- Imports a module, preserving the initial state on failure.
--
handleImport :: C.ImportedModule -> PSCI ()
handleImport im = do
   st <- updateImportedModules im <$> PSCI (lift get)
   let m = createTemporaryModuleForImports st
   e <- psciIO . runMake $ P.make modulesDir (psciLoadedModules st ++ [(Left P.RebuildAlways, m)]) []
   case e of
     Left err -> PSCI $ outputStrLn err
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
  e <- psciIO . runMake $ P.make modulesDir (psciLoadedModules st ++ [(Left P.RebuildAlways, m)]) []
  case e of
    Left err -> PSCI $ outputStrLn err
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
        showType _ = error "The impossible happened in printModuleSignatures."

-- |
-- Browse a module and displays its signature (if module exists).
--
handleBrowse :: P.ModuleName -> PSCI ()
handleBrowse moduleName = do
  st <- PSCI $ lift get
  let loadedModules = psciLoadedModules st
  env <- psciIO . runMake $ P.make modulesDir loadedModules []
  case env of
    Left err -> PSCI $ outputStrLn err
    Right env' ->
      if moduleName `notElem` (nub . map ((\ (P.Module _ modName _ _ ) -> modName) . snd)) loadedModules
        then PSCI $ outputStrLn $ "Module '" ++ N.runModuleName moduleName ++ "' is not valid."
        else printModuleSignatures moduleName env'

-- |
-- Takes a value and prints its kind
--
handleKindOf :: P.Type -> PSCI ()
handleKindOf typ = do
  st <- PSCI $ lift get
  let m = createTemporaryModuleForKind st typ
      mName = P.ModuleName [P.ProperName "$PSCI"]
  e <- psciIO . runMake $ P.make modulesDir (psciLoadedModules st ++ [(Left P.RebuildAlways, m)]) []
  case e of
    Left err -> PSCI $ outputStrLn err
    Right env' ->
      case M.lookup (P.Qualified (Just mName) $ P.ProperName "IT") (P.typeSynonyms env') of
        Just (_, typ') -> do
          let chk = P.CheckState env' 0 0 (Just mName)
              k   = L.runStateT (P.unCheck (P.kindOf mName typ')) chk
          case k of
            Left errStack   -> PSCI . outputStrLn . P.prettyPrintMultipleErrors False $ errStack
            Right (kind, _) -> PSCI . outputStrLn . P.prettyPrintKind $ kind
        Nothing -> PSCI $ outputStrLn "Could not find kind"

-- Commands

-- |
-- Parses the input and returns either a Metacommand or an expression.
--
getCommand :: Bool -> InputT (StateT PSCiState IO) (Either String (Maybe C.Command))
getCommand singleLineMode = do
  firstLine <- getInputLine "> "
  case firstLine of
    Nothing -> return (Right Nothing)
    Just "" -> return (Right Nothing)
    Just s | singleLineMode || head s == ':' -> return . either Left (Right . Just) $ parseCommand s
    Just s -> either Left (Right . Just) . parseCommand <$> go [s]
  where
    go :: [String] -> InputT (StateT PSCiState IO) String
    go ls = maybe (return . unlines $ reverse ls) (go . (:ls)) =<< getInputLine "  "

-- |
-- Performs an action for each meta-command given, and also for expressions..
--
handleCommand :: C.Command -> PSCI ()
handleCommand (C.Expression val) = handleDeclaration val
handleCommand C.Help = PSCI $ outputStrLn helpMessage
handleCommand (C.Import im) = handleImport im
handleCommand (C.Decls l) = handleDecls l
handleCommand (C.LoadFile filePath) = do
  absPath <- psciIO $ expandTilde filePath
  exists <- psciIO $ doesFileExist absPath
  if exists then do
    PSCI . lift $ modify (updateImportedFiles absPath)
    m <- psciIO $ loadModule absPath
    case m of
      Left err -> PSCI $ outputStrLn err
      Right mods -> PSCI . lift $ modify (updateModules (map ((,) (Right absPath)) mods))
  else
    PSCI . outputStrLn $ "Couldn't locate: " ++ filePath
handleCommand C.Reset = do
  files <- psciImportedFilenames <$> PSCI (lift get)
  PSCI . lift . modify $ \st -> st
    { psciImportedFilenames   = files
    , psciImportedModules     = defaultImports
    , psciLetBindings         = []
    }
  loadAllImportedModules
handleCommand (C.TypeOf val) = handleTypeOf val
handleCommand (C.KindOf typ) = handleKindOf typ
handleCommand (C.Browse moduleName) = handleBrowse moduleName
handleCommand (C.Show "loaded") = handleShowLoadedModules
handleCommand (C.Show "import") = handleShowImportedModules
handleCommand _ = PSCI $ outputStrLn "Unknown command"

loadUserConfig :: IO (Maybe [C.Command])
loadUserConfig = do
  configFile <- (</> ".psci") <$> getCurrentDirectory
  exists <- doesFileExist configFile
  if exists
  then do
    ls <- lines <$> readFile configFile
    case mapM parseCommand ls of
      Left err -> print err >> exitFailure
      Right cs -> return $ Just cs
  else
    return Nothing

-- |
-- The PSCI main loop.
--
loop :: PSCiOptions -> IO ()
loop PSCiOptions{..} = do
  config <- loadUserConfig
  modulesOrFirstError <- loadAllModules psciInputFile
  case modulesOrFirstError of
    Left err -> print err >> exitFailure
    Right modules -> do
      historyFilename <- getHistoryFilename
      let settings = defaultSettings { historyFile = Just historyFilename }
      flip evalStateT (PSCiState psciInputFile defaultImports modules [] psciInputNodeFlags) . runInputT (setComplete completion settings) $ do
        outputStrLn prologueMessage
        traverse_ (mapM_ (runPSCI . handleCommand)) config
        go
      where
        go :: InputT (StateT PSCiState IO) ()
        go = do
          c <- getCommand (not psciMultiLineMode)
          case c of
            Left err -> outputStrLn err >> go
            Right Nothing -> go
            Right (Just C.Quit) -> outputStrLn quitMessage
            Right (Just c') -> runPSCI (loadAllImportedModules >> handleCommand c') >> go

multiLineMode :: Parser Bool
multiLineMode = switch $
     long "multi-line-mode"
  <> short 'm'
  <> Opts.help "Run in multi-line mode (use ^D to terminate commands)"

inputFile :: Parser FilePath
inputFile = strArgument $
     metavar "FILE"
  <> Opts.help "Optional .purs files to load on start"

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
                          <*> nodeFlagsFlag

main :: IO ()
main = execParser opts >>= loop
  where
  opts        = info (version <*> helper <*> psciOptions) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "psci - Interactive mode for PureScript"
  footerInfo  = footer $ "psci " ++ showVersion Paths.version

  version :: Parser (a -> a)
  version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> Opts.help "Show the version number" <> hidden

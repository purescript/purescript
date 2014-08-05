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

{-# LANGUAGE DoAndIfThenElse, FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Main where

import Commands

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.State.Strict
import qualified Control.Monad.Trans.State.Lazy as L
import Control.Monad.Error (ErrorT(..), MonadError)
import Control.Monad.Error.Class (MonadError(..))

import Data.List (intercalate, isPrefixOf, nub, sortBy)
import Data.Maybe (mapMaybe)
import Data.Foldable (traverse_)
import Data.Version (showVersion)
import Data.Traversable (traverse)

import Parser

import System.IO.Error (tryIOError)
import System.Console.Haskeline
import System.Directory
       (createDirectoryIfMissing, getModificationTime, doesFileExist,
        findExecutable, getHomeDirectory, getCurrentDirectory)
import System.Process (readProcessWithExitCode)
import System.Exit
import System.Environment.XDG.BaseDir
import System.FilePath
       (pathSeparator, takeDirectory, (</>), isPathSeparator)
import qualified System.Console.CmdTheLine as Cmd

import Text.Parsec (ParseError)

import qualified Data.Map as M
import qualified Language.PureScript as P
import qualified Paths_purescript as Paths
import qualified System.IO.UTF8 as U
       (writeFile, putStrLn, print, readFile)

-- |
-- The PSCI state.
-- Holds a list of imported modules, loaded files, and partial let bindings.
-- The let bindings are partial,
-- because it makes more sense to apply the binding to the final evaluated expression.
--
data PSCiState = PSCiState
  { psciImportedFilenames   :: [FilePath]
  , psciImportedModuleNames :: [P.ModuleName]
  , psciLoadedModules       :: [(FilePath, P.Module)]
  , psciLetBindings         :: [P.Value -> P.Value]
  }

-- State helpers

-- |
-- Updates the state to have more imported modules.
--
updateImportedFiles :: FilePath -> PSCiState -> PSCiState
updateImportedFiles filename st = st { psciImportedFilenames = filename : psciImportedFilenames st }

-- |
-- Updates the state to have more imported modules.
--
updateImports :: P.ModuleName -> PSCiState -> PSCiState
updateImports name st = st { psciImportedModuleNames = name : psciImportedModuleNames st }

-- |
-- Updates the state to have more loaded files.
--
updateModules :: [(FilePath, P.Module)] -> PSCiState -> PSCiState
updateModules modules st = st { psciLoadedModules = psciLoadedModules st ++ modules }

-- |
-- Updates the state to have more let bindings.
--
updateLets :: (P.Value -> P.Value) -> PSCiState -> PSCiState
updateLets name st = st { psciLetBindings = name : psciLetBindings st }

-- File helpers
-- |
-- Load the necessary modules.
--
defaultImports :: [P.ModuleName]
defaultImports = [P.ModuleName [P.ProperName "Prelude"]]

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
getHistoryFilename = getUserConfigFile "purescript" "psci_history"

-- |
-- Grabs the filename where prelude is.
--
getPreludeFilename :: IO FilePath
getPreludeFilename = Paths.getDataFileName "prelude/prelude.purs"

-- |
-- Loads a file for use with imports.
--
loadModule :: FilePath -> IO (Either String [P.Module])
loadModule filename = either (Left . show) Right . P.runIndentParser filename P.parseModules <$> U.readFile filename

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
  intercalate "\n    " (map (intercalate "    ") help)

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
  , ""
  , "Expressions are terminated using Ctrl+D"
  ]

-- |
-- The quit message.
--
quitMessage :: String
quitMessage = "See ya!"

-- Haskeline completions

-- |
-- Loads module, function, and file completions.
--
completion :: CompletionFunc (StateT PSCiState IO)
completion = completeWord Nothing " \t\n\r" findCompletions
  where
  findCompletions :: String -> StateT PSCiState IO [Completion]
  findCompletions str = do
    ms <- map snd . psciLoadedModules <$> get
    files <- listFiles str
    let matches = filter (isPrefixOf str) (names ms)
    return $ sortBy sorter $ map simpleCompletion matches ++ files
  getDeclName :: Maybe [P.DeclarationRef] -> P.Declaration -> Maybe P.Ident
  getDeclName Nothing (P.ValueDeclaration ident _ _ _ _) = Just ident
  getDeclName (Just exts) (P.ValueDeclaration ident _ _ _ _) | isExported = Just ident
    where
    isExported = any exports exts
    exports (P.ValueRef ident') = ident == ident'
    exports (P.PositionedDeclarationRef _ r) = exports r
    exports _ = False
  getDeclName exts (P.PositionedDeclaration _ d) = getDeclName exts d
  getDeclName _ _ = Nothing
  names :: [P.Module] -> [String]
  names ms = nub [ show qual
              | P.Module moduleName ds exts <- ms
              , ident <- mapMaybe (getDeclName exts) ds
              , qual <- [ P.Qualified Nothing ident
                        , P.Qualified (Just moduleName) ident]
              ]
  sorter :: Completion -> Completion -> Ordering
  sorter (Completion _ d1 _) (Completion _ d2 _) = compare d1 d2

-- Compilation

-- | Compilation options.
--
options :: P.Options
options = P.Options False True False True Nothing True Nothing [] [] False

-- |
-- PSCI monad
--
newtype PSCI a = PSCI { runPSCI :: InputT (StateT PSCiState IO) a } deriving (Functor, Applicative, Monad)

psciIO :: IO a -> PSCI a
psciIO io = PSCI (lift (lift io))

newtype Make a = Make { unMake :: ErrorT String IO a } deriving (Functor, Applicative, Monad, MonadError String)

runMake :: Make a -> IO (Either String a)
runMake = runErrorT . unMake

makeIO :: IO a -> Make a
makeIO = Make . ErrorT . fmap (either (Left . show) Right) . tryIOError

instance P.MonadMake Make where
  getTimestamp path = makeIO $ do
    exists <- doesFileExist path
    case exists of
      True -> Just <$> getModificationTime path
      False -> return Nothing
  readTextFile path = makeIO $ U.readFile path
  writeTextFile path text = makeIO $ do
    mkdirp path
    U.writeFile path text
  liftError = either throwError return
  progress s = unless (s == "Compiling Main") $ makeIO . U.putStrLn $ s

mkdirp :: FilePath -> IO ()
mkdirp = createDirectoryIfMissing True . takeDirectory

-- |
-- Makes a volatile module to execute the current expression.
--
createTemporaryModule :: Bool -> PSCiState -> P.Value -> P.Module
createTemporaryModule exec PSCiState{psciImportedModuleNames = imports, psciLetBindings = lets} value =
  let
    moduleName = P.ModuleName [P.ProperName "$PSCI"]
    importDecl m = P.ImportDeclaration m Nothing Nothing
    traceModule = P.ModuleName [P.ProperName "Debug", P.ProperName "Trace"]
    trace = P.Var (P.Qualified (Just traceModule) (P.Ident "print"))
    itValue = foldl (\x f -> f x) value lets
    mainValue = P.App trace (P.Var (P.Qualified Nothing (P.Ident "it")))
    itDecl = P.ValueDeclaration (P.Ident "it") P.Value [] Nothing itValue
    mainDecl = P.ValueDeclaration (P.Ident "main") P.Value [] Nothing mainValue
    decls = if exec then [itDecl, mainDecl] else [itDecl]
  in
    P.Module moduleName ((importDecl `map` imports) ++ decls) Nothing

-- |
-- Makes a volatile module to hold a non-qualified type synonym for a fully-qualified data type declaration.
--
createTemporaryModuleForKind :: PSCiState -> P.Type -> P.Module
createTemporaryModuleForKind PSCiState{psciImportedModuleNames = imports} typ =
  let
    moduleName = P.ModuleName [P.ProperName "$PSCI"]
    importDecl m = P.ImportDeclaration m Nothing Nothing
    itDecl = P.TypeSynonymDeclaration (P.ProperName "IT") [] typ
  in
    P.Module moduleName ((importDecl `map` imports) ++ [itDecl]) Nothing

modulesDir :: FilePath
modulesDir = ".psci_modules" ++ pathSeparator : "node_modules"

indexFile :: FilePath
indexFile = ".psci_modules" ++ pathSeparator : "index.js"

-- |
-- Takes a value declaration and evaluates it with the current state.
--
handleDeclaration :: P.Value -> PSCI ()
handleDeclaration value = do
  st <- PSCI $ lift get
  let m = createTemporaryModule True st value
  e <- psciIO . runMake $ P.make modulesDir options (psciLoadedModules st ++ [("$PSCI.purs", m)])
  case e of
    Left err -> PSCI $ outputStrLn err
    Right _ -> do
      psciIO $ writeFile indexFile $ "require('$PSCI').main();"
      process <- psciIO findNodeProcess
      result  <- psciIO $ traverse (\node -> readProcessWithExitCode node [indexFile] "") process
      case result of
        Just (ExitSuccess,   out, _)   -> PSCI $ outputStrLn out
        Just (ExitFailure _, _,   err) -> PSCI $ outputStrLn err
        Nothing                        -> PSCI $ outputStrLn "Couldn't find node.js"

-- |
-- Takes a value and prints its type
--
handleTypeOf :: P.Value -> PSCI ()
handleTypeOf value = do
  st <- PSCI $ lift get
  let m = createTemporaryModule False st value
  e <- psciIO . runMake $ P.make modulesDir options (psciLoadedModules st ++ [("$PSCI.purs", m)])
  case e of
    Left err -> PSCI $ outputStrLn err
    Right env' ->
      case M.lookup (P.ModuleName [P.ProperName "$PSCI"], P.Ident "it") (P.names env') of
        Just (ty, _, _) -> PSCI . outputStrLn . P.prettyPrintType $ ty
        Nothing -> PSCI $ outputStrLn "Could not find type"

-- |
-- Takes a value and prints its kind
--
handleKindOf :: P.Type -> PSCI ()
handleKindOf typ = do
  st <- PSCI $ lift get
  let m = createTemporaryModuleForKind st typ
      mName = P.ModuleName [P.ProperName "$PSCI"]
  e <- psciIO . runMake $ P.make modulesDir options (psciLoadedModules st ++ [("$PSCI.purs", m)])
  case e of
    Left err -> PSCI $ outputStrLn err
    Right env' ->
      case M.lookup (P.Qualified (Just mName) $ P.ProperName "IT") (P.typeSynonyms env') of
        Just (_, typ') -> do
          let chk = P.CheckState env' 0 0 (Just mName)
              k   = L.runStateT (P.unCheck (P.kindOf mName typ')) chk
          case k of 
            Left errStack   -> PSCI . outputStrLn . P.prettyPrintErrorStack False $ errStack
            Right (kind, _) -> PSCI . outputStrLn . P.prettyPrintKind $ kind
        Nothing -> PSCI $ outputStrLn "Could not find kind"

-- Commands

-- |
-- Parses the input and returns either a Metacommand or an expression.
--
getCommand :: InputT (StateT PSCiState IO) (Either ParseError (Maybe Command))
getCommand = do
  firstLine <- getInputLine "> "
  case firstLine of
    Nothing -> return (Right Nothing)
    Just "" -> return (Right Nothing)
    Just s@ (':' : _) -> return . either Left (Right . Just) $ parseCommand s -- The start of a command
    Just s -> either Left (Right . Just) . parseCommand <$> go [s]
  where
    go :: [String] -> InputT (StateT PSCiState IO) String
    go ls = maybe (return . unlines $ reverse ls) (go . (:ls)) =<< getInputLine "  "

-- |
-- Performs an action for each meta-command given, and also for expressions..
--
handleCommand :: Command -> PSCI ()
handleCommand (Expression val) = handleDeclaration val
handleCommand Help = PSCI $ outputStrLn helpMessage
handleCommand (Import moduleName) = PSCI $ lift $ modify (updateImports moduleName)
handleCommand (Let l) = PSCI $ lift $ modify (updateLets l)
handleCommand (LoadFile filePath) = do
  absPath <- psciIO $ expandTilde filePath
  exists <- psciIO $ doesFileExist absPath
  if exists then do
    PSCI . lift $ modify (updateImportedFiles absPath)
    m <- psciIO $ loadModule absPath
    case m of
      Left err -> PSCI $ outputStrLn err
      Right mods -> PSCI . lift $ modify (updateModules (map ((,) absPath) mods))
  else
    PSCI . outputStrLn $ "Couldn't locate: " ++ filePath
handleCommand Reset = do
  files <- psciImportedFilenames <$> PSCI (lift get)
  filesAndModules <- mapM (\file -> fmap (fmap (map ((,) file))) . psciIO . loadModule $ file) files
  let modulesOrFirstError = fmap concat $ sequence filesAndModules
  case modulesOrFirstError of
    Left err -> psciIO $ putStrLn err >> exitFailure
    Right modules -> PSCI . lift $ put (PSCiState files defaultImports modules [])
handleCommand (TypeOf val) = handleTypeOf val
handleCommand (KindOf typ) = handleKindOf typ
handleCommand _ = PSCI $ outputStrLn "Unknown command"

inputFiles :: Cmd.Term [FilePath]
inputFiles = Cmd.value $ Cmd.posAny [] $ Cmd.posInfo { Cmd.posName = "file(s)"
                                                     , Cmd.posDoc = "Optional .purs files to load on start" }

loadUserConfig :: IO (Maybe [Command])
loadUserConfig = do
  configFile <- (</> ".psci") <$> getCurrentDirectory
  exists <- doesFileExist configFile
  if exists
  then do
    ls <- lines <$> U.readFile configFile
    case mapM parseCommand ls of
      Left err -> U.print err >> exitFailure
      Right cs -> return $ Just cs
  else
    return Nothing

-- |
-- The PSCI main loop.
--
loop :: [FilePath] -> IO ()
loop files = do
  config <- loadUserConfig
  preludeFilename <- getPreludeFilename
  filesAndModules <- mapM (\file -> fmap (fmap (map ((,) file))) . loadModule $ file) (preludeFilename : files)
  let modulesOrFirstError = fmap concat $ sequence filesAndModules
  case modulesOrFirstError of
    Left err -> putStrLn err >> exitFailure
    Right modules -> do
      historyFilename <- getHistoryFilename
      let settings = defaultSettings {historyFile = Just historyFilename}
      flip evalStateT (PSCiState (preludeFilename : files) defaultImports modules []) . runInputT (setComplete completion settings) $ do
        outputStrLn prologueMessage
        traverse_ (mapM_ (runPSCI . handleCommand)) config
        go
      where
        go :: InputT (StateT PSCiState IO) ()
        go = do
          c <- getCommand
          case c of
            Left err -> outputStrLn (show err) >> go
            Right Nothing -> go
            Right (Just Quit) -> outputStrLn quitMessage
            Right (Just c') -> runPSCI (handleCommand c') >> go

term :: Cmd.Term (IO ())
term = loop <$> inputFiles

termInfo :: Cmd.TermInfo
termInfo = Cmd.defTI
  { Cmd.termName = "psci"
  , Cmd.version  = showVersion Paths.version
  , Cmd.termDoc  = "Interactive mode for PureScript"
  }

main :: IO ()
main = Cmd.run (term, termInfo)

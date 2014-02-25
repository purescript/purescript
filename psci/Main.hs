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

{-# LANGUAGE DoAndIfThenElse, FlexibleContexts #-}

module Main where

import Commands

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.State.Strict

import Data.List (intercalate, isPrefixOf, nub, sortBy)
import Data.Maybe (mapMaybe)
import Data.Traversable (traverse)
import Data.Version (showVersion)

import Parser

import System.Console.Haskeline
import System.Directory (doesFileExist, findExecutable, getHomeDirectory)
import System.Exit
import System.Environment.XDG.BaseDir
import System.FilePath ((</>), isPathSeparator)
import qualified System.Console.CmdTheLine as Cmd
import System.Process

import Text.Parsec (ParseError)

import qualified Data.Map as M
import qualified Language.PureScript as P
import qualified Paths_purescript as Paths
import qualified System.IO.UTF8 as U (readFile)

-- |
-- The PSCI state.
-- Holds a list of imported modules, loaded files, and partial let bindings.
-- The let bindings are partial,
-- because it makes more sense to apply the binding to the final evaluated expression.
--
data PSCiState = PSCiState
  { psciImportedFilenames   :: [FilePath]
  , psciImportedModuleNames :: [P.ModuleName]
  , psciLoadedModules       :: [P.Module]
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
updateModules :: [P.Module] -> PSCiState -> PSCiState
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
    ms <- psciLoadedModules <$> get
    files <- listFiles str
    let matches = filter (isPrefixOf str) (names ms)
    return $ sortBy sorter $ map simpleCompletion matches ++ files
  getDeclName :: P.Declaration -> Maybe P.Ident
  getDeclName (P.ValueDeclaration ident _ _ _) = Just ident
  getDeclName _ = Nothing
  names :: [P.Module] -> [String]
  names ms = nub [ show qual
              | P.Module moduleName ds <- ms
              , ident <- mapMaybe getDeclName ds
              , qual <- [ P.Qualified Nothing ident
                        , P.Qualified (Just moduleName) ident]
              ]
  sorter :: Completion -> Completion -> Ordering
  sorter (Completion _ d1 _) (Completion _ d2 _) = compare d1 d2

-- Compilation

-- | Compilation options.
--
options :: P.Options
options = P.Options True False True (Just "Main") True "PS" []

-- |
-- Makes a volatile module to execute the current expression.
--
createTemporaryModule :: Bool -> PSCiState -> P.Value -> P.Module
createTemporaryModule exec PSCiState{psciImportedModuleNames = imports, psciLetBindings = lets} value =
  let
    moduleName = P.ModuleName [P.ProperName "Main"]
    importDecl m = P.ImportDeclaration m Nothing
    traceModule = P.ModuleName [P.ProperName "Debug", P.ProperName "Trace"]
    trace = P.Var (P.Qualified (Just traceModule) (P.Ident "print"))
    itValue = foldl (\x f -> f x) value lets
    mainValue = P.App trace (P.Var (P.Qualified Nothing (P.Ident "it")))
    itDecl = P.ValueDeclaration (P.Ident "it") [] Nothing itValue
    mainDecl = P.ValueDeclaration (P.Ident "main") [] Nothing mainValue
    decls = if exec then [itDecl, mainDecl] else [itDecl]
  in
    P.Module moduleName $ map importDecl imports ++ decls

-- |
-- Takes a value declaration and evaluates it with the current state.
--
handleDeclaration :: P.Value -> PSCiState -> InputT (StateT PSCiState IO) ()
handleDeclaration value st = do
  let m = createTemporaryModule True st value
  case P.compile options (psciLoadedModules st ++ [m]) of
    Left err -> outputStrLn err
    Right (js, _, _) -> do
      process <- lift . lift $ findNodeProcess
      result  <- lift . lift $ traverse (\node -> readProcessWithExitCode node [] js) process
      case result of
        Just (ExitSuccess,   out, _)   -> outputStrLn out
        Just (ExitFailure _, _,   err) -> outputStrLn err
        Nothing                        -> outputStrLn "Couldn't find node.js"

-- |
-- Takes a value and prints its type
--
handleTypeOf :: P.Value -> PSCiState -> InputT (StateT PSCiState IO) ()
handleTypeOf value st = do
  let m = createTemporaryModule False st value
  case P.compile options { P.optionsMain = Nothing } (psciLoadedModules st ++ [m]) of
    Left err -> outputStrLn err
    Right (_, _, env') ->
      case M.lookup (P.ModuleName [P.ProperName "Main"], P.Ident "it") (P.names env') of
        Just (ty, _) -> outputStrLn . P.prettyPrintType $ ty
        Nothing -> outputStrLn "Could not find type"

-- Commands

-- |
-- Parses the input and returns either a Metacommand or an expression.
--
getCommand :: InputT (StateT PSCiState IO) (Either ParseError (Maybe Command))
getCommand = do
  firstLine <- getInputLine "> "
  case firstLine of
    Nothing -> return (Right Nothing)
    Just s@ (':' : _) -> return . either Left (Right . Just) $ parseCommand s -- The start of a command
    Just s -> either Left (Right . Just) . parseCommand <$> go [s]
  where
    go :: [String] -> InputT (StateT PSCiState IO) String
    go ls = maybe (return . unlines $ reverse ls) (go . (:ls)) =<< getInputLine "  "

-- |
-- Performs an action for each meta-command given, and also for expressions..
--
handleCommand :: Command -> InputT (StateT PSCiState IO) ()
handleCommand (Expression val) = lift get >>= handleDeclaration val
handleCommand Help = outputStrLn helpMessage
handleCommand (Import moduleName) = lift $ modify (updateImports moduleName)
handleCommand (Let l) = lift $ modify (updateLets l)
handleCommand (LoadFile filePath) = do
  absPath <- lift . lift $ expandTilde filePath
  exists <- lift . lift $ doesFileExist absPath
  if exists then do
    lift $ modify (updateImportedFiles absPath)
    either outputStrLn (lift . modify . updateModules) =<< (lift . lift $ loadModule absPath)
  else
    outputStrLn $ "Couldn't locate: " ++ filePath
handleCommand Reset = do
  files <- psciImportedFilenames <$> lift get
  modulesOrFirstError <- fmap concat . sequence <$> mapM (lift . lift . loadModule) files
  case modulesOrFirstError of
    Left err -> lift . lift $ putStrLn err >> exitFailure
    Right modules -> lift $ put (PSCiState files defaultImports modules [])
handleCommand (TypeOf val) = lift get >>= handleTypeOf val
handleCommand _ = outputStrLn "Unknown command"

inputFiles :: Cmd.Term [FilePath]
inputFiles = Cmd.value $ Cmd.posAny [] $ Cmd.posInfo { Cmd.posName = "file(s)"
                                                     , Cmd.posDoc = "Optional .purs files to load on start" }

-- |
-- The PSCI main loop.
--
loop :: [FilePath] -> IO ()
loop files = do
  preludeFilename <- getPreludeFilename
  modulesOrFirstError <- fmap concat . sequence <$> mapM loadModule (preludeFilename : files)
  case modulesOrFirstError of
    Left err -> putStrLn err >> exitFailure
    Right modules -> do
      historyFilename <- getHistoryFilename
      let settings = defaultSettings {historyFile = Just historyFilename}
      flip evalStateT (PSCiState (preludeFilename : files) defaultImports modules []) . runInputT (setComplete completion settings) $ do
        outputStrLn prologueMessage
        go
      where
        go :: InputT (StateT PSCiState IO) ()
        go = do
          c <- getCommand
          case c of
            Left err -> outputStrLn (show err) >> go
            Right Nothing -> go
            Right (Just Quit) -> outputStrLn quitMessage
            Right (Just c') -> handleCommand c' >> go

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


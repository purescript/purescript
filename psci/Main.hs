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
import Control.Monad.Trans.State

import Data.List (intercalate, isPrefixOf, nub, sortBy)
import Data.Maybe (mapMaybe)
import Data.Traversable (traverse)

import Parser

import System.Console.Haskeline
import System.Directory (doesFileExist, findExecutable, getHomeDirectory)
import System.Exit
import System.Environment.XDG.BaseDir
import System.FilePath ((</>), isPathSeparator)
import System.Process

import Text.Parsec (choice)

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
data PSCI = PSCI [P.ModuleName] [P.Module] [P.Value -> P.Value]

-- State helpers

-- |
-- Synonym to be more descriptive.
-- This is just @lift@
--
inputTToState :: InputT IO a -> StateT PSCI (InputT IO) a
inputTToState = lift

-- |
-- Synonym to be more descriptive.
-- This is just @lift . lift@
--
ioToState :: IO a -> StateT PSCI (InputT IO) a
ioToState = lift . lift

-- |
-- Updates the state to have more imported modules.
--
updateImports :: P.ModuleName -> PSCI -> PSCI
updateImports name (PSCI i m b) = PSCI (name : i) m b

-- |
-- Updates the state to have more loaded files.
--
updateModules :: [P.Module] -> PSCI -> PSCI
updateModules modules (PSCI i m b) = PSCI i (m ++ modules) b

-- |
-- Updates the state to have more let bindings.
--
updateLets :: (P.Value -> P.Value) -> PSCI -> PSCI
updateLets name (PSCI i m b) = PSCI i m (b ++ [name])

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
loadModule = fmap (either (Left . show) Right . parseModules) . U.readFile

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
completion :: [P.Module] -> CompletionFunc IO
completion ms = completeWord Nothing " \t\n\r" findCompletions
  where
  findCompletions :: String -> IO [Completion]
  findCompletions str = do
    files <- listFiles str
    let matches = filter (isPrefixOf str) names
    return $ sortBy sorter $ map simpleCompletion matches ++ files
  getDeclName :: P.Declaration -> Maybe P.Ident
  getDeclName (P.ValueDeclaration ident _ _ _) = Just ident
  getDeclName _ = Nothing
  names :: [String]
  names = nub [ show qual
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
createTemporaryModule :: Bool -> [P.ModuleName] -> [P.Value -> P.Value] -> P.Value -> P.Module
createTemporaryModule exec imports lets value =
  let
    moduleName = P.ModuleName [P.ProperName "Main"]
    importDecl m = P.ImportDeclaration m Nothing
    traceModule = P.ModuleName [P.ProperName "Trace"]
    trace = P.Var (P.Qualified (Just traceModule) (P.Ident "print"))
    value' = foldr ($) value lets
    itDecl = P.ValueDeclaration (P.Ident "it") [] Nothing value'
    mainDecl = P.ValueDeclaration (P.Ident "main") [] Nothing (P.App trace (P.Var (P.Qualified Nothing (P.Ident "it"))))
  in
    P.Module moduleName $ map importDecl imports ++ if exec then [itDecl, mainDecl] else [itDecl]

-- |
-- Takes a value declaration and evaluates it with the current state.
--
handleDeclaration :: P.Value -> PSCI -> InputT IO ()
handleDeclaration value (PSCI imports loadedModules lets) = do
  let m = createTemporaryModule True imports lets value
  case P.compile options (loadedModules ++ [m]) of
    Left err -> outputStrLn err
    Right (js, _, _) -> do
      process <- lift findNodeProcess
      result <- lift $ traverse (\node -> readProcessWithExitCode node [] js) process
      case result of
        Just (ExitSuccess,   out, _)   -> outputStrLn out
        Just (ExitFailure _, _,   err) -> outputStrLn err
        Nothing                        -> outputStrLn "Couldn't find node.js"

-- |
-- Takes a value and prints its type
--
handleTypeOf :: P.Value -> PSCI -> InputT IO ()
handleTypeOf value (PSCI imports loadedModules lets) = do
  let m = createTemporaryModule False imports lets value
  case P.compile options { P.optionsMain = Nothing } (loadedModules ++ [m]) of
    Left err -> outputStrLn err
    Right (_, _, env') ->
      case M.lookup (P.ModuleName [P.ProperName "Main"], P.Ident "it") (P.names env') of
        Just (ty, _) -> outputStrLn . P.prettyPrintType $ ty
        Nothing -> outputStrLn "Could not find type"

-- Commands

-- |
-- Parses the input and returns either a Metacommand or an expression.
--
getCommand :: InputT IO Command
getCommand = do
  firstLine <- getInputLine "> "
  case firstLine of
    Nothing   -> return Empty
    Just line -> case parseCommands line of
      Left err -> return $ Unknown err
      Right c  -> case c of
        Expression expr -> Expression <$> go [expr]
        _               -> lift $ return c
  where
    go :: [String] -> InputT IO String
    go ls = maybe (return . unlines $ reverse ls) (go . (:ls)) =<< getInputLine "  "

-- |
-- Performs an action for each meta-command given, and also for expressions..
--
handleCommand :: Command -> StateT PSCI (InputT IO) ()
handleCommand Empty = return ()
handleCommand (Expression ls) =
  case psciParser (choice [Left <$> psciLet, Right <$> psciExpression]) ls of
    Left  err          -> inputTToState $ outputStrLn (show err)
    Right (Left l)     -> modify (updateLets l)
    Right (Right decl) -> get >>= inputTToState . handleDeclaration decl
handleCommand Help = inputTToState $ outputStrLn helpMessage
handleCommand (Import moduleName) = modify (updateImports moduleName)
handleCommand (LoadFile filePath) = do
  absPath <- ioToState $ expandTilde filePath
  exists <- ioToState $ doesFileExist absPath
  if exists then
    either outputTStrLn (modify . updateModules) =<< ioToState (loadModule absPath)
  else
    outputTStrLn $ "Couldn't locate: " ++ filePath
handleCommand Reload = do
  (Right prelude) <- ioToState $ loadModule =<< getPreludeFilename
  put (PSCI defaultImports prelude [])
handleCommand (TypeOf expr) =
  case parseExpression expr of
    Left err    -> inputTToState $ outputStrLn (show err)
    Right expr' -> get >>= inputTToState . handleTypeOf expr'
handleCommand _ = outputTStrLn "Unknown command"

-- Command helpers

-- |
-- Lifts the output call to the StateT.
--
outputTStrLn :: String -> StateT PSCI (InputT IO) ()
outputTStrLn = inputTToState . outputStrLn

-- |
-- The corresponding @print@ version for the StateT.
--
outprintT :: Show a => a -> StateT PSCI (InputT IO) ()
outprintT = outputTStrLn . show

-- |
-- The PSCI main loop.
--
main :: IO ()
main = do
  preludeFilename <- getPreludeFilename
  (Right prelude) <- loadModule preludeFilename
  historyFilename <- getHistoryFilename
  let settings = defaultSettings {historyFile = Just historyFilename}
  runInputT (setComplete (completion prelude) settings) $ do
    outputStrLn prologueMessage
    evalStateT go (PSCI defaultImports prelude [])
  where
    go :: StateT PSCI (InputT IO) ()
    go = do
      c <- inputTToState getCommand
      case c of
        Quit -> outputTStrLn quitMessage
        _    -> handleCommand c >> go

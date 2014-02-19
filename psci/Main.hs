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
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Main where

import Commands

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.State

import Data.List (intercalate, isPrefixOf, nub, sort)
import Data.Maybe (mapMaybe)
import Data.Traversable (traverse)

import System.Console.Haskeline
import System.Directory (findExecutable)
import System.Exit
import System.Environment.XDG.BaseDir
import System.Process

import qualified Language.PureScript as P
import qualified Paths_purescript as Paths
import qualified System.IO.UTF8 as U (readFile)
import qualified Text.Parsec as Parsec (Parsec, eof)

data PSCI = PSCI [P.ProperName] [P.Module] [P.Value -> P.Value]

-- State helpers
inputTToState :: InputT IO a -> StateT PSCI (InputT IO) a
inputTToState = lift

ioToState :: IO a -> StateT PSCI (InputT IO) a
ioToState = lift . lift

updateImports :: String -> PSCI -> PSCI
updateImports name (PSCI i m b) = PSCI (i ++ [P.ProperName name]) m b

updateModules :: [P.Module] -> PSCI -> PSCI
updateModules modules (PSCI i m b) = PSCI i (m ++ modules) b

updateBinders :: (P.Value -> P.Value) -> PSCI -> PSCI
updateBinders name (PSCI i m b) = PSCI i m (b ++ [name])

-- File helpers
defaultImports :: [P.ProperName]
defaultImports = [P.ProperName "Prelude", P.ProperName "Eff"]

findNodeProcess :: IO (Maybe String)
findNodeProcess = runMaybeT . msum $ map (MaybeT . findExecutable) names
    where names = ["nodejs", "node"]

getHistoryFilename :: IO FilePath
getHistoryFilename = getUserConfigFile "purescript" "psci_history"

getPreludeFilename :: IO FilePath
getPreludeFilename = Paths.getDataFileName "prelude/prelude.purs"

loadModule :: FilePath -> IO (Either String [P.Module])
loadModule moduleFile = do
  moduleText <- U.readFile moduleFile
  return . either (Left . show) Right $ P.runIndentParser "" P.parseModules moduleText

-- Messages
helpMessage :: String
helpMessage = "The following commands are available:\n\n    " ++
  intercalate "\n    " (map (intercalate "    ") help)

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

quitMessage :: String
quitMessage = "See ya!"

-- Haskeline completions
completion :: [P.Module] -> CompletionFunc IO
completion ms = completeWord Nothing " \t\n\r" findCompletions
  where
  findCompletions :: String -> IO [Completion]
  findCompletions str = do
    files <- listFiles str
    let names = nub [ show qual
                    | P.Module moduleName ds <- ms
                    , ident <- mapMaybe getDeclName ds
                    , qual <- [ P.Qualified Nothing ident
                              , P.Qualified (Just moduleName) ident]
                    ]
    let matches = sort $ filter (isPrefixOf str) names
    return $ map simpleCompletion matches ++ files
  getDeclName :: P.Declaration -> Maybe P.Ident
  getDeclName (P.ValueDeclaration ident _ _ _) = Just ident
  getDeclName _ = Nothing

-- Compilation
options :: P.Options
options = P.Options True False True (Just "Main") True "PS" []

createTemporaryModule :: [P.ProperName] -> [P.Value -> P.Value] -> P.Value -> P.Module
createTemporaryModule imports binders value =
  let
    moduleName = P.ModuleName [P.ProperName "Main"]
    importDecl m = P.ImportDeclaration m Nothing
    traceModule = P.ModuleName [P.ProperName "Trace"]
    trace = P.Var (P.Qualified (Just traceModule) (P.Ident "print"))
    value' = foldr ($) value binders
    mainDecl = P.ValueDeclaration (P.Ident "main") [] Nothing (P.App trace value')
  in
    P.Module moduleName $ map (importDecl . P.ModuleName . return) imports ++ [mainDecl]

handleDeclaration :: P.Value -> PSCI -> InputT IO ()
handleDeclaration value (PSCI imports loadedModules binders) = do
  let m = createTemporaryModule imports binders value
  case P.compile options (loadedModules ++ [m]) of
    Left err -> outputStrLn err
    Right (js, _, _) -> do
      process <- lift findNodeProcess
      result <- lift $ traverse (\node -> readProcessWithExitCode node [] js) process
      case result of
        Just (ExitSuccess,   out, _)   -> outputStrLn out
        Just (ExitFailure _, _,   err) -> outputStrLn err
        Nothing                        -> outputStrLn "Couldn't find node.js"

-- Parser helpers
parseDoNotationLet :: Parsec.Parsec String P.ParseState (P.Value -> P.Value)
parseDoNotationLet = P.Let <$> (P.reserved "let" *> P.indented *> P.parseBinder)
                           <*> (P.indented *> P.reservedOp "=" *> P.parseValue)

parseExpression :: Parsec.Parsec String P.ParseState P.Value
parseExpression = P.whiteSpace *> P.parseValue <* Parsec.eof

-- Commands
handleCommand :: Command -> StateT PSCI (InputT IO) ()
handleCommand Empty = return ()
handleCommand (Expression ls) =
  case P.runIndentParser "" parseDoNotationLet (unlines ls) of
    Left _ ->
      case P.runIndentParser "" parseExpression (unlines ls) of
        Left err -> inputTToState $ outputStrLn (show err)
        Right decl -> get >>= inputTToState . handleDeclaration decl
    Right binder -> modify (updateBinders binder)
handleCommand Help = inputTToState $ outputStrLn helpMessage
handleCommand (Import moduleName) = modify (updateImports moduleName)
handleCommand (LoadFile filePath) = do
  mf <- ioToState $ loadModule filePath
  case mf of
    Left err -> inputTToState $ outputStrLn err
    Right mf' -> modify (updateModules mf')
handleCommand Reload = do
  (Right prelude) <- ioToState $ getPreludeFilename >>= loadModule
  put (PSCI defaultImports prelude [])
handleCommand Unknown = inputTToState $ outputStrLn "Unknown command"

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
        Quit -> inputTToState $ outputStrLn quitMessage
        _    -> handleCommand c >> go

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

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Data.List (nub, isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Traversable (traverse)

import System.Console.Haskeline
import System.Directory (createDirectoryIfMissing, findExecutable)
import System.Exit
import System.Environment.XDG.BaseDir
import System.Process

import qualified Language.PureScript as P
import qualified Paths_purescript as Paths
import qualified System.IO.UTF8 as U (readFile)
import qualified Text.Parsec as Parsec (eof)

getPreludeFilename :: IO FilePath
getPreludeFilename = Paths.getDataFileName "prelude/prelude.purs"

options :: P.Options
options = P.Options True False True (Just "Main") True "PS" []

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
    let matches = filter (isPrefixOf str) names
    return $ map simpleCompletion matches ++ files
  getDeclName :: P.Declaration -> Maybe P.Ident
  getDeclName (P.ValueDeclaration ident _ _ _) = Just ident
  getDeclName _ = Nothing

createTemporaryModule :: [P.ProperName] -> P.Value -> P.Module
createTemporaryModule imports value =
  let
    moduleName = P.ModuleName [P.ProperName "Main"]
    importDecl m = P.ImportDeclaration m Nothing
    traceModule = P.ModuleName [P.ProperName "Trace"]
    trace = P.Var (P.Qualified (Just traceModule) (P.Ident "print"))
    mainDecl = P.ValueDeclaration (P.Ident "main") [] Nothing
        (P.Do [ P.DoNotationBind (P.VarBinder (P.Ident "it")) value
              , P.DoNotationValue (P.App trace (P.Var (P.Qualified Nothing (P.Ident "it"))) )
              ])
  in
    P.Module moduleName $ map (importDecl . P.ModuleName . return) imports ++ [mainDecl]

handleDeclaration :: [P.Module] -> [P.ProperName] -> P.Value -> InputT IO ()
handleDeclaration loadedModules imports value = do
  let m = createTemporaryModule imports value
  case P.compile options (loadedModules ++ [m]) of
    Left err -> outputStrLn err
    Right (js, _, _) -> do
      process <- lift findNodeProcess
      result <- lift $ traverse (\node -> readProcessWithExitCode node [] js) process
      case result of
        Just (ExitSuccess,   out, _)   -> outputStrLn out
        Just (ExitFailure _, _,   err) -> outputStrLn err
        Nothing                        -> outputStrLn "Couldn't find node.js"

data Command
  = Empty
  | Expression [String]
  | Import String
  | LoadModule FilePath
  | Reload deriving (Show, Eq)

getCommand :: InputT IO Command
getCommand = do
  firstLine <- getInputLine  "> "
  case firstLine of
    Nothing -> return Empty
    Just (':':'i':' ':moduleName) -> return $ Import moduleName
    Just (':':'m':' ':modulePath) -> return $ LoadModule modulePath
    Just ":r" -> return Reload
    Just (':':_) -> outputStrLn "Unknown command" >> getCommand
    Just other -> Expression <$> go [other]
  where
  go ls = do
    l <- getInputLine "  "
    case l of
      Nothing -> return $ reverse ls
      Just l' -> go (l' : ls)

loadModule :: FilePath -> IO (Either String [P.Module])
loadModule moduleFile = do
  print moduleFile
  moduleText <- U.readFile moduleFile
  return . either (Left . show) Right $ P.runIndentParser "" P.parseModules moduleText

findNodeProcess :: IO (Maybe String)
findNodeProcess = runMaybeT . msum $ map (MaybeT . findExecutable) names
    where names = ["nodejs", "node"]

getHistoryFilename :: IO FilePath
getHistoryFilename = do
  purescriptConfig <- getUserConfigDir "purescript"
  createDirectoryIfMissing True purescriptConfig
  getUserConfigFile "purescript" "psci_history"

main :: IO ()
main = do
  preludeFilename <- getPreludeFilename
  (Right prelude) <- loadModule preludeFilename
  historyFilename <- getHistoryFilename
  let settings = defaultSettings {historyFile = Just historyFilename}
  runInputT (setComplete (completion prelude) settings) $ do
    outputStrLn " ____                 ____            _       _   "
    outputStrLn "|  _ \\ _   _ _ __ ___/ ___|  ___ _ __(_)_ __ | |_ "
    outputStrLn "| |_) | | | | '__/ _ \\___ \\ / __| '__| | '_ \\| __|"
    outputStrLn "|  __/| |_| | | |  __/___) | (__| |  | | |_) | |_ "
    outputStrLn "|_|    \\__,_|_|  \\___|____/ \\___|_|  |_| .__/ \\__|"
    outputStrLn "                                       |_|        "
    outputStrLn ""
    outputStrLn "Expressions are terminated using Ctrl+D"
    go [P.ProperName "Prelude", P.ProperName "Eff"] prelude
  where
  go imports loadedModules = do
    cmd <- getCommand
    case cmd of
      Empty -> go imports loadedModules
      Expression ls -> do
        case P.runIndentParser "" (P.whiteSpace *> P.parseValue <* Parsec.eof) (unlines ls) of
          Left err -> outputStrLn (show err)
          Right decl -> handleDeclaration loadedModules imports decl
        go imports loadedModules
      Import moduleName -> go (imports ++ [P.ProperName moduleName]) loadedModules
      LoadModule moduleFile -> do
        ms <- lift $ loadModule moduleFile
        case ms of
          Left err -> outputStrLn err
          Right ms' -> go imports (loadedModules ++ ms')
      Reload -> do
        preludeFilename <- lift getPreludeFilename
        (Right prelude) <- lift $ loadModule preludeFilename
        go [P.ProperName "Prelude"] prelude



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

{-# LANGUAGE DoAndIfThenElse #-}

module Main (main) where

import qualified Language.PureScript as P

import Data.List (isSuffixOf)
import Data.Traversable (traverse)
import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import System.Exit
import System.Process
import System.FilePath (pathSeparator)
import System.Directory (getCurrentDirectory, getTemporaryDirectory, getDirectoryContents, findExecutable)
import Text.Parsec (ParseError)
import qualified Paths_purescript as Paths
import qualified System.IO.UTF8 as U

preludeFilename :: IO FilePath
preludeFilename = Paths.getDataFileName "prelude/prelude.purs"

readInput :: [FilePath] -> IO (Either ParseError [P.Module])
readInput inputFiles = fmap (fmap concat . sequence) $ forM inputFiles $ \inputFile -> do
  text <- U.readFile inputFile
  return $ P.runIndentParser inputFile P.parseModules text

compile :: P.Options -> [FilePath] -> IO (Either String (String, String, P.Environment))
compile opts inputFiles = do
  modules <- readInput inputFiles
  case modules of
    Left parseError ->
      return (Left $ show parseError)
    Right ms -> return $ P.compile opts ms

assert :: FilePath -> P.Options -> FilePath -> (Either String (String, String, P.Environment) -> IO (Maybe String)) -> IO ()
assert preludeExterns opts inputFile f = do
  e <- compile opts [preludeExterns, inputFile]
  maybeErr <- f e
  case maybeErr of
    Just err -> putStrLn err >> exitFailure
    Nothing -> return ()

assertCompiles :: String -> FilePath -> FilePath -> IO ()
assertCompiles preludeJs preludeExterns inputFile = do
  putStrLn $ "Assert " ++ inputFile ++ " compiles successfully"
  let options = P.defaultOptions { P.optionsMain = Just "Main", P.optionsModules = ["Main"], P.optionsCodeGenModules = ["Main"], P.optionsBrowserNamespace = Just "Tests" }
  assert preludeExterns options inputFile $ either (return . Just) $ \(js, _, _) -> do
    process <- findNodeProcess
    putStrLn $ preludeJs ++ js
    result <- traverse (\node -> readProcessWithExitCode node [] (preludeJs ++ js)) process
    case result of
      Just (ExitSuccess, out, _) -> putStrLn out >> return Nothing
      Just (ExitFailure _, _, err) -> return $ Just err
      Nothing -> return $ Just "Couldn't find node.js executable"

assertDoesNotCompile :: FilePath -> FilePath -> IO ()
assertDoesNotCompile preludeExterns inputFile = do
  putStrLn $ "Assert " ++ inputFile ++ " does not compile"
  assert preludeExterns (P.defaultOptions { P.optionsBrowserNamespace = Just "Tests" }) inputFile $ \e ->
    case e of
      Left _ -> return Nothing
      Right _ -> return $ Just "Should not have compiled"

findNodeProcess :: IO (Maybe String)
findNodeProcess = runMaybeT . msum $ map (MaybeT . findExecutable) names
    where names = ["nodejs", "node"]

main :: IO ()
main = do
  prelude <- preludeFilename
  putStrLn "Compiling Prelude"
  preludeResult <- compile (P.defaultOptions { P.optionsBrowserNamespace = Just "Tests" }) [prelude]
  case preludeResult of
    Left err -> putStrLn err >> exitFailure
    Right (preludeJs, exts, _) -> do
      tmp <- getTemporaryDirectory
      let preludeExterns = tmp ++ pathSeparator : "prelude.externs"
      writeFile preludeExterns exts
      putStrLn $ "Wrote " ++ preludeExterns
      cd <- getCurrentDirectory
      let examples = cd ++ pathSeparator : "examples"
      let passing = examples ++ pathSeparator : "passing"
      passingTestCases <- getDirectoryContents passing
      forM_ passingTestCases $ \inputFile -> when (".purs" `isSuffixOf` inputFile) $
        assertCompiles preludeJs preludeExterns (passing ++ pathSeparator : inputFile)
      let failing = examples ++ pathSeparator : "failing"
      failingTestCases <- getDirectoryContents failing
      forM_ failingTestCases $ \inputFile -> when (".purs" `isSuffixOf` inputFile) $
        assertDoesNotCompile preludeExterns (failing ++ pathSeparator : inputFile)
      exitSuccess

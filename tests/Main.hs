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
import Control.Applicative
import Control.Monad
import System.Exit
import System.Process
import System.FilePath (pathSeparator)
import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.Environment (getArgs)
import Text.Parsec (ParseError)
import qualified Paths_purescript as Paths
import qualified System.IO.UTF8 as U
import qualified Data.Map as M

preludeFilename :: IO FilePath
preludeFilename = Paths.getDataFileName "libraries/prelude/prelude.purs"

readInput :: [FilePath] -> IO (Either ParseError [P.Module])
readInput inputFiles = fmap (fmap concat . sequence) $ forM inputFiles $ \inputFile -> do
  text <- U.readFile inputFile
  return $ P.runIndentParser P.parseModules text

compile :: P.Options -> [FilePath] -> IO (Either String String)
compile opts inputFiles = do
  modules <- readInput inputFiles
  case modules of
    Left parseError -> do
      return (Left $ show parseError)
    Right ms -> do
      case P.compile opts ms of
        Left typeError -> do
          return (Left typeError)
        Right (js, _, _) -> do
          return (Right js)

assert :: P.Options -> [FilePath] -> (Either String String -> IO (Maybe String)) -> IO ()
assert opts inputFiles f = do
  e <- compile opts inputFiles
  maybeErr <- f e
  case maybeErr of
    Just err -> putStrLn err >> exitFailure
    Nothing -> return ()

assertCompiles :: FilePath -> IO ()
assertCompiles inputFile = do
  putStrLn $ "assert " ++ inputFile ++ " compiles successfully"
  prelude <- preludeFilename
  assert (P.defaultOptions { P.optionsRunMain = True }) [prelude, inputFile] $ either (return . Just) $ \js -> do
    args <- getArgs
    if "--run-js" `elem` args
    then do
      (exitCode, out, err) <- readProcessWithExitCode "nodejs" [] js
      case exitCode of
        ExitSuccess -> putStrLn out >> return Nothing
        ExitFailure code -> return $ Just err
    else return Nothing

assertDoesNotCompile :: FilePath -> IO ()
assertDoesNotCompile inputFile = do
  putStrLn $ "assert " ++ inputFile ++ " does not compile"
  assert P.defaultOptions [inputFile] $ \e ->
    case e of
      Left _ -> return Nothing
      Right _ -> return $ Just "Should not have compiled"

main :: IO ()
main = do
  cd <- getCurrentDirectory
  putStrLn $ cd
  let examples = cd ++ pathSeparator : "examples"
  let passing = examples ++ pathSeparator : "passing"
  passingTestCases <- getDirectoryContents passing
  forM_ passingTestCases $ \inputFile -> when (".purs" `isSuffixOf` inputFile) $
    assertCompiles (passing ++ pathSeparator : inputFile)
  let failing = examples ++ pathSeparator : "failing"
  failingTestCases <- getDirectoryContents failing
  forM_ failingTestCases $ \inputFile -> when (".purs" `isSuffixOf` inputFile) $
    assertDoesNotCompile (failing ++ pathSeparator : inputFile)
  exitSuccess

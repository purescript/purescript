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

module Main (main) where

import PureScript
import PureScript.CodeGen.Pretty.Types

import Data.List (isSuffixOf)
import Control.Applicative
import Control.Monad
import System.Exit (exitSuccess, exitFailure)
import System.FilePath (pathSeparator)
import System.Directory (getCurrentDirectory, getDirectoryContents)
import qualified System.IO.UTF8 as U
import qualified Data.Map as M

compile :: FilePath -> IO (Either String Environment)
compile inputFile = do
  ast <- runIndentParser parseDeclarations <$> U.readFile inputFile
  case ast of
    Left parseError -> do
      return (Left $ show parseError)
    Right decls -> do
      case check (typeCheckAll decls) of
        Left typeError -> do
          return (Left typeError)
        Right (_, env) -> do
          return (Right env)

assert :: FilePath -> (Either String Environment -> Maybe String) -> IO ()
assert inputFile f = do
  e <- compile inputFile
  case f e of
    Just err -> exitFailure
    Nothing -> return ()

assertCompiles :: FilePath -> IO ()
assertCompiles inputFile = do
  putStrLn $ "assert " ++ inputFile ++ " compiles successfully"
  assert inputFile $ either Just (const Nothing)

assertDoesNotCompile :: FilePath -> IO ()
assertDoesNotCompile inputFile = do
  putStrLn $ "assert " ++ inputFile ++ " does not compile"
  assert inputFile $ either (const Nothing) (const $ Just "Should not have compiled")

main :: IO ()
main = do
  cd <- getCurrentDirectory
  let examples = cd ++ pathSeparator : "examples"
  let passing = examples ++ pathSeparator : "passing"
  passingTestCases <- getDirectoryContents passing
  forM_ passingTestCases $ \inputFile -> when (".ps" `isSuffixOf` inputFile) $
    assertCompiles (passing ++ pathSeparator : inputFile)
  let failing = examples ++ pathSeparator : "failing"
  failingTestCases <- getDirectoryContents failing
  forM_ failingTestCases $ \inputFile -> when (".ps" `isSuffixOf` inputFile) $
    assertDoesNotCompile (failing ++ pathSeparator : inputFile)
  exitSuccess

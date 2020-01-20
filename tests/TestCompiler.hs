{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCompiler where

-- Failing tests in the `failing` and `warning` folders check their output
-- agains the relative golden files (`.out`).
-- To golden files are generated automatically when missing, and can be updated
-- by passing `--accept` to `--test-arguments.`

import Prelude ()
import Prelude.Compat

import qualified Language.PureScript as P

import Data.Function (on)
import Data.List (minimumBy)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.Map as M

import qualified Data.ByteString.Lazy as BS

import Control.Monad

import System.Exit
import System.Process
import System.FilePath
import System.IO

import TestUtils
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty (testGroup)
import Test.Tasty.Golden (goldenVsString)

main :: IO TestTree
main = do
  (supportModules, supportExterns, supportForeigns) <- setupSupportModules
  passing <- passingTests supportModules supportExterns supportForeigns
  warning <- warningTests supportModules supportExterns supportForeigns
  failing <- failingTests supportModules supportExterns supportForeigns
  return . testGroup "compiler" $ [passing, warning, failing]

passingTests
  :: [P.Module]
  -> [P.ExternsFile]
  -> M.Map P.ModuleName FilePath
  -> IO TestTree
passingTests supportModules supportExterns supportForeigns = do
  passingTestCases <- getTestFiles "passing"

  outputFile <- createOutputFile logfile

  testSpec "Passing examples" $
    forM_ passingTestCases $ \testPurs ->
      it ("'" <> takeFileName (getTestMain testPurs) <> "' should compile and run without error") $
        assertCompiles supportModules supportExterns supportForeigns testPurs outputFile

warningTests
  :: [P.Module]
  -> [P.ExternsFile]
  -> M.Map P.ModuleName FilePath
  -> IO TestTree
warningTests supportModules supportExterns supportForeigns = do
  warningTestCases <- getTestFiles "warning"
  return $ testGroup "Warning examples" $
    assertGolden supportModules supportExterns supportForeigns warningTestCases

failingTests
  :: [P.Module]
  -> [P.ExternsFile]
  -> M.Map P.ModuleName FilePath
  -> IO TestTree
failingTests supportModules supportExterns supportForeigns = do
  failingTestCases <- getTestFiles "failing"
  return $ testGroup "Failing examples" $
    assertGolden supportModules supportExterns supportForeigns failingTestCases

assertCompiles
  :: [P.Module]
  -> [P.ExternsFile]
  -> M.Map P.ModuleName FilePath
  -> [FilePath]
  -> Handle
  -> Expectation
assertCompiles supportModules supportExterns supportForeigns inputFiles outputFile =
  assert supportModules supportExterns supportForeigns inputFiles checkMain $ \e ->
    case e of
      Left errs -> return . Just . P.prettyPrintMultipleErrors P.defaultPPEOptions $ errs
      Right _ -> do
        process <- findNodeProcess
        let entryPoint = modulesDir </> "index.js"
        writeFile entryPoint "require('Main').main()"
        result <- traverse (\node -> readProcessWithExitCode node [entryPoint] "") process
        hPutStrLn outputFile $ "\n" <> takeFileName (last inputFiles) <> ":"
        case result of
          Just (ExitSuccess, out, err)
            | not (null err) -> return $ Just $ "Test wrote to stderr:\n\n" <> err
            | not (null out) && trim (last (lines out)) == "Done" -> do
                hPutStr outputFile out
                return Nothing
            | otherwise -> return $ Just $ "Test did not finish with 'Done':\n\n" <> out
          Just (ExitFailure _, _, err) -> return $ Just err
          Nothing -> return $ Just "Couldn't find node.js executable"

assertGolden
  :: [P.Module]
  -> [P.ExternsFile]
  -> M.Map P.ModuleName FilePath
  -> [[FilePath]]
  -> [TestTree]
assertGolden supportModules supportExterns supportForeigns testCases =
  flip fmap testCases $ \files ->
    let mainFile = head files
    in
      goldenVsString
      (takeBaseName mainFile)
      (replaceExtension mainFile ".out")
      (BS.fromStrict . T.encodeUtf8 . T.pack <$> printErrorOrWarning supportModules supportExterns supportForeigns files)

printErrorOrWarning
  :: [P.Module]
  -> [P.ExternsFile]
  -> M.Map P.ModuleName FilePath
  -> [FilePath]
  -> IO String
printErrorOrWarning supportModules supportExterns supportForeigns inputFiles = do
  (e, w) <- compile supportModules supportExterns supportForeigns inputFiles noPreCheck
  case (const w <$> e) of
    Left errs ->
      return $ P.prettyPrintMultipleErrors P.defaultPPEOptions $ errs
    Right warnings ->
      return $ P.prettyPrintMultipleErrors P.defaultPPEOptions $ warnings
  where
    noPreCheck = const (return ())

getTestMain :: [FilePath] -> FilePath
getTestMain = minimumBy (compare `on` length)

logfile :: FilePath
logfile = "psc-tests.out"

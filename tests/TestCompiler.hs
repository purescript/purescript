{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCompiler where

-- Failing tests can specify the kind of error that should be thrown with a
-- @shouldFailWith declaration. For example:
--
--   "-- @shouldFailWith TypesDoNotUnify"
--
-- will cause the test to fail unless that module fails to compile with exactly
-- one TypesDoNotUnify error.
--
-- If a module is expected to produce multiple type errors, then use multiple
-- @shouldFailWith lines; for example:
--
--   -- @shouldFailWith TypesDoNotUnify
--   -- @shouldFailWith TypesDoNotUnify
--   -- @shouldFailWith TransitiveExportError

import Prelude ()
import Prelude.Compat

import qualified Language.PureScript as P

import Control.Arrow ((>>>))
import Data.Function (on)
import Data.List (sort, stripPrefix, intercalate, minimumBy)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

import qualified Data.Map as M

import Control.Monad

import System.Exit
import System.Process
import System.FilePath
import System.IO
import System.IO.UTF8 (readUTF8File)

import TestUtils
import Test.Tasty
import Test.Tasty.Hspec

main :: IO TestTree
main = testSpec "compiler" spec

spec :: Spec
spec = do
  (supportModules, supportExterns, supportForeigns) <- runIO $ setupSupportModules

  (passingTestCases, warningTestCases, failingTestCases) <- runIO $
    (,,) <$> getTestFiles "passing"
         <*> getTestFiles "warning"
         <*> getTestFiles "failing"

  outputFile <- runIO $ createOutputFile logfile

  context "Passing examples" $
    forM_ passingTestCases $ \testPurs ->
      it ("'" <> takeFileName (getTestMain testPurs) <> "' should compile and run without error") $
        assertCompiles supportModules supportExterns supportForeigns testPurs outputFile

  context "Warning examples" $
    forM_ warningTestCases $ \testPurs -> do
      let mainPath = getTestMain testPurs
      expectedWarnings <- runIO $ getShouldWarnWith mainPath
      it ("'" <> takeFileName mainPath <> "' should compile with warning(s) '" <> intercalate "', '" expectedWarnings <> "'") $
        assertCompilesWithWarnings supportModules supportExterns supportForeigns testPurs expectedWarnings

  context "Failing examples" $
    forM_ failingTestCases $ \testPurs -> do
      let mainPath = getTestMain testPurs
      expectedFailures <- runIO $ getShouldFailWith mainPath
      it ("'" <> takeFileName mainPath <> "' should fail with '" <> intercalate "', '" expectedFailures <> "'") $
        assertDoesNotCompile supportModules supportExterns supportForeigns testPurs expectedFailures

  where

  -- Takes the test entry point from a group of purs files - this is determined
  -- by the file with the shortest path name, as everything but the main file
  -- will be under a subdirectory.
  getTestMain :: [FilePath] -> FilePath
  getTestMain = minimumBy (compare `on` length)

  -- Scans a file for @shouldFailWith directives in the comments, used to
  -- determine expected failures
  getShouldFailWith :: FilePath -> IO [String]
  getShouldFailWith = extractPragma "shouldFailWith"

  -- Scans a file for @shouldWarnWith directives in the comments, used to
  -- determine expected warnings
  getShouldWarnWith :: FilePath -> IO [String]
  getShouldWarnWith = extractPragma "shouldWarnWith"

  extractPragma :: String -> FilePath -> IO [String]
  extractPragma pragma = fmap go . readUTF8File
    where
      go = lines >>> mapMaybe (stripPrefix ("-- @" ++ pragma ++ " ")) >>> map trim


checkShouldFailWith :: [String] -> P.MultipleErrors -> Maybe String
checkShouldFailWith expected errs =
  let actual = map P.errorCode $ P.runMultipleErrors errs
  in if sort expected == sort (map T.unpack actual)
    then checkPositioned errs
    else Just $ "Expected these errors: " ++ show expected ++ ", but got these: "
      ++ show actual ++ ", full error messages: \n"
      ++ unlines (map (P.renderBox . P.prettyPrintSingleError P.defaultPPEOptions) (P.runMultipleErrors errs))

checkPositioned :: P.MultipleErrors -> Maybe String
checkPositioned errs =
  case mapMaybe guardSpans (P.runMultipleErrors errs) of
    [] ->
      Nothing
    errs' ->
      Just
        $ "Found errors with missing source spans:\n"
        ++ unlines (map (P.renderBox . P.prettyPrintSingleError P.defaultPPEOptions) errs')
  where
  guardSpans :: P.ErrorMessage -> Maybe P.ErrorMessage
  guardSpans err = case P.errorSpan err of
    Just ss | any (not . isNonsenseSpan) ss -> Nothing
    _ -> Just err

  isNonsenseSpan :: P.SourceSpan -> Bool
  isNonsenseSpan (P.SourceSpan spanName spanStart spanEnd) =
    spanName == "" || spanName == "<module>" || (spanStart == emptyPos && spanEnd == emptyPos)

  emptyPos :: P.SourcePos
  emptyPos = P.SourcePos 0 0

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

assertCompilesWithWarnings
  :: [P.Module]
  -> [P.ExternsFile]
  -> M.Map P.ModuleName FilePath
  -> [FilePath]
  -> [String]
  -> Expectation
assertCompilesWithWarnings supportModules supportExterns supportForeigns inputFiles shouldWarnWith =
  assert supportModules supportExterns supportForeigns inputFiles checkMain $ \e ->
    case e of
      Left errs ->
        return . Just . P.prettyPrintMultipleErrors P.defaultPPEOptions $ errs
      Right warnings ->
        return
          . fmap (printAllWarnings warnings)
          $ checkShouldFailWith shouldWarnWith warnings

  where
  printAllWarnings warnings =
    (<> "\n\n" <> P.prettyPrintMultipleErrors P.defaultPPEOptions warnings)

assertDoesNotCompile
  :: [P.Module]
  -> [P.ExternsFile]
  -> M.Map P.ModuleName FilePath
  -> [FilePath]
  -> [String]
  -> Expectation
assertDoesNotCompile supportModules supportExterns supportForeigns inputFiles shouldFailWith =
  assert supportModules supportExterns supportForeigns inputFiles noPreCheck $ \e ->
    case e of
      Left errs ->
        return $ if null shouldFailWith
          then Just $ "shouldFailWith declaration is missing (errors were: "
                      ++ show (map P.errorCode (P.runMultipleErrors errs))
                      ++ ")"
          else checkShouldFailWith shouldFailWith errs
      Right _ ->
        return $ Just "Should not have compiled"

  where
  noPreCheck = const (return ())

logfile :: FilePath
logfile = "psc-tests.out"

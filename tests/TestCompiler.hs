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
--
-- Failing tests also check their output against the relative golden files (`.out`).
-- The golden files are generated automatically when missing, and can be updated
-- by passing `--accept` to `--test-arguments.`

import Prelude ()
import Prelude.Compat

import qualified Language.PureScript as P

import Control.Arrow ((>>>))
import Data.Function (on)
import Data.List (sort, stripPrefix, intercalate, minimumBy)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.Map as M

import qualified Data.ByteString.Lazy as BS

import Control.Monad

import System.Exit
import System.Process
import System.FilePath
import System.IO
import System.IO.UTF8 (readUTF8File)

import Text.Regex.Base
import Text.Regex.TDFA (Regex)

import TestUtils
import Test.Tasty
import Test.Tasty.Hspec
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
  tests <- forM warningTestCases $ \testPurs -> do
    let mainPath = getTestMain testPurs
    expectedWarnings <- getShouldWarnWith mainPath
    wTc <- testSpecs $
      it ("'" <> takeFileName mainPath <> "' should compile with warning(s) '" <> intercalate "', '" expectedWarnings <> "'") $
        assertCompilesWithWarnings supportModules supportExterns supportForeigns testPurs expectedWarnings
    return $ wTc ++ [ goldenVsString
                      ("'" <> takeFileName mainPath <> "' golden test")
                      (replaceExtension mainPath ".out")
                      (BS.fromStrict . T.encodeUtf8 . T.pack <$> printErrorOrWarning supportModules supportExterns supportForeigns testPurs)
                    ]
  return $ testGroup "Warning examples" $ concat tests

failingTests
  :: [P.Module]
  -> [P.ExternsFile]
  -> M.Map P.ModuleName FilePath
  -> IO TestTree
failingTests supportModules supportExterns supportForeigns = do
  failingTestCases <- getTestFiles "failing"
  tests <- forM failingTestCases $ \testPurs -> do
    let mainPath = getTestMain testPurs
    expectedFailures <- getShouldFailWith mainPath
    fTc <- testSpecs $
      it ("'" <> takeFileName mainPath <> "' should fail with '" <> intercalate "', '" expectedFailures <> "'") $
        assertDoesNotCompile supportModules supportExterns supportForeigns testPurs expectedFailures
    return $ fTc ++ [ goldenVsString
                    ("'" <> takeFileName mainPath <> "' golden test")
                    (replaceExtension mainPath ".out")
                    (BS.fromStrict . T.encodeUtf8 . T.pack <$> printErrorOrWarning supportModules supportExterns supportForeigns testPurs)
                  ]
  return $ testGroup "Failing examples" $ concat tests

checkShouldReport :: [String] -> (P.MultipleErrors -> String) -> P.MultipleErrors -> Maybe String
checkShouldReport expected prettyPrintDiagnostics errs =
  let actual = map P.errorCode $ P.runMultipleErrors errs
  in if sort expected == sort (map T.unpack actual)
    then checkPositioned errs
    else Just $ "Expected these diagnostics: " ++ show expected ++ ", but got these: "
      ++ show actual ++ ", full diagnostic messages: \n"
      ++ prettyPrintDiagnostics errs

checkPositioned :: P.MultipleErrors -> Maybe String
checkPositioned errs =
  case mapMaybe guardSpans (P.runMultipleErrors errs) of
    [] ->
      Nothing
    errs' ->
      Just
        $ "Found diagnostics with missing source spans:\n"
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
        return $ checkShouldReport shouldWarnWith (P.prettyPrintMultipleWarnings P.defaultPPEOptions) warnings

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
          else checkShouldReport shouldFailWith (P.prettyPrintMultipleErrors P.defaultPPEOptions) errs
      Right _ ->
        return $ Just "Should not have compiled"

  where
  noPreCheck = const (return ())

printErrorOrWarning
  :: [P.Module]
  -> [P.ExternsFile]
  -> M.Map P.ModuleName FilePath
  -> [FilePath]
  -> IO String
printErrorOrWarning supportModules supportExterns supportForeigns inputFiles = do
  -- Sorting the input files makes some messages (e.g., duplicate module) deterministic
  (res, warnings) <- compile supportModules supportExterns supportForeigns (sort inputFiles) noPreCheck
  return . normalizePaths $ case res of
    Left errs ->
      P.prettyPrintMultipleErrors P.defaultPPEOptions errs
    Right _ ->
      P.prettyPrintMultipleWarnings P.defaultPPEOptions warnings
  where
    noPreCheck = const (return ())

-- Replaces Windows-style paths in an error or warning with POSIX paths
normalizePaths :: String -> String
normalizePaths = if pathSeparator == '\\'
  then replaceMatches " [0-9A-Za-z_-]+(\\\\[0-9A-Za-z_-]+)+\\.[A-Za-z]+\\>" (map turnSlash)
  else id
  where
    turnSlash '\\' = '/'
    turnSlash c = c

-- Uses a function to replace all matches of a regular expression in a string
replaceMatches :: String -> (String -> String) -> String -> String
replaceMatches reString phi = go
  where
    re :: Regex
    re = makeRegex reString
    go :: String -> String
    go haystack =
      let (prefix, needle, suffix) = match re haystack
      in prefix ++ (if null needle then "" else phi needle ++ go suffix)

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


logfile :: FilePath
logfile = "psc-tests.out"

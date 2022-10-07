{-# LANGUAGE DoAndIfThenElse #-}

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
-- Warning and failing tests also check their output against the relative
-- golden files (`.out`). The golden files are generated automatically when
-- missing, and can be updated by setting the "HSPEC_ACCEPT" environment
-- variable, e.g. by running `HSPEC_ACCEPT=true stack test`.

import Prelude

import qualified Language.PureScript as P
import Language.PureScript.Interactive.IO (readNodeProcessWithExitCode)

import Control.Arrow ((>>>))
import qualified Data.ByteString as BS
import Data.Function (on)
import Data.List (sort, stripPrefix, minimumBy)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


import Control.Monad

import System.Exit
import System.FilePath
import System.IO
import System.IO.UTF8 (readUTF8File)

import Text.Regex.Base
import Text.Regex.TDFA (Regex)

import TestUtils
import Test.Hspec

spec :: SpecWith SupportModules
spec = do
  passingTests
  warningTests
  failingTests
  optimizeTests

passingTests :: SpecWith SupportModules
passingTests = do
  passingTestCases <- runIO $ getTestFiles "passing"

  describe "Passing examples" $
    beforeAllWith ((<$> createOutputFile logfile) . (,)) $
      forM_ passingTestCases $ \testPurs ->
        it ("'" <> takeFileName (getTestMain testPurs) <> "' should compile and run without error") $ \(support, outputFile) ->
          assertCompiles support testPurs outputFile

warningTests :: SpecWith SupportModules
warningTests = do
  warningTestCases <- runIO $ getTestFiles "warning"

  describe "Warning examples" $
    forM_ warningTestCases $ \testPurs -> do
      let mainPath = getTestMain testPurs
      it ("'" <> takeFileName mainPath <> "' should compile with expected warning(s)") $ \support -> do
        expectedWarnings <- getShouldWarnWith mainPath
        assertCompilesWithWarnings support testPurs expectedWarnings

failingTests :: SpecWith SupportModules
failingTests = do
  failingTestCases <- runIO $ getTestFiles "failing"

  describe "Failing examples" $ do
    forM_ failingTestCases $ \testPurs -> do
      let mainPath = getTestMain testPurs
      it ("'" <> takeFileName mainPath <> "' should fail to compile") $ \support -> do
        expectedFailures <- getShouldFailWith mainPath
        assertDoesNotCompile support testPurs expectedFailures

optimizeTests :: SpecWith SupportModules
optimizeTests = do
  optimizeTestCases <- runIO $ getTestFiles "optimize"

  describe "Optimization examples" $
    forM_ optimizeTestCases $ \testPurs ->
      it ("'" <> takeFileName (getTestMain testPurs) <> "' should compile to expected output") $ \support ->
        assertCompilesToExpectedOutput support testPurs

checkShouldReport :: [String] -> (P.MultipleErrors -> String) -> P.MultipleErrors -> Expectation
checkShouldReport expected prettyPrintDiagnostics errs =
  let actual = map P.errorCode $ P.runMultipleErrors errs
  in if sort expected == sort (map T.unpack actual)
    then checkPositioned errs
    else expectationFailure $ "Expected these diagnostics: " ++ show expected ++ ", but got these: "
      ++ show actual ++ ", full diagnostic messages: \n"
      ++ prettyPrintDiagnostics errs

checkPositioned :: P.MultipleErrors -> Expectation
checkPositioned errs =
  case mapMaybe guardSpans (P.runMultipleErrors errs) of
    [] ->
      pure ()
    errs' ->
      expectationFailure
        $ "Found diagnostics with missing source spans:\n"
        ++ unlines (map (P.renderBox . P.prettyPrintSingleError P.defaultPPEOptions) errs')
  where
  guardSpans :: P.ErrorMessage -> Maybe P.ErrorMessage
  guardSpans err = case P.errorSpan err of
    Just ss | not $ all isNonsenseSpan ss -> Nothing
    _ -> Just err

  isNonsenseSpan :: P.SourceSpan -> Bool
  isNonsenseSpan (P.SourceSpan spanName spanStart spanEnd) =
    spanName == "" || spanName == "<module>" || (spanStart == emptyPos && spanEnd == emptyPos)

  emptyPos :: P.SourcePos
  emptyPos = P.SourcePos 0 0

assertCompiles
  :: SupportModules
  -> [FilePath]
  -> Handle
  -> Expectation
assertCompiles support inputFiles outputFile = do
  (fileContents, (result, _)) <- compile (Just IsMain) support inputFiles
  let errorOptions = P.defaultPPEOptions { P.ppeFileContents = fileContents }
  case result of
    Left errs -> expectationFailure . P.prettyPrintMultipleErrors errorOptions $ errs
    Right _ -> do
      let entryPoint = modulesDir </> "index.js"
      writeFile entryPoint "import('./Main/index.js').then(({ main }) => main());"
      nodeResult <- readNodeProcessWithExitCode Nothing [entryPoint] ""
      hPutStrLn outputFile $ "\n" <> takeFileName (last inputFiles) <> ":"
      case nodeResult of
        Right (ExitSuccess, out, err)
          | not (null err) -> expectationFailure $ "Test wrote to stderr:\n\n" <> err
          | not (null out) && trim (last (lines out)) == "Done" -> hPutStr outputFile out
          | otherwise -> expectationFailure $ "Test did not finish with 'Done':\n\n" <> out
        Right (ExitFailure _, _, err) -> expectationFailure err
        Left err -> expectationFailure err

assertCompilesWithWarnings
  :: SupportModules
  -> [FilePath]
  -> [String]
  -> Expectation
assertCompilesWithWarnings support inputFiles shouldWarnWith = do
  (fileContents, result'@(result, warnings)) <- compile Nothing support inputFiles
  let errorOptions = P.defaultPPEOptions { P.ppeFileContents = fileContents }
  case result of
    Left errs ->
      expectationFailure . P.prettyPrintMultipleErrors errorOptions $ errs
    Right _ -> do
      checkShouldReport shouldWarnWith (P.prettyPrintMultipleWarnings errorOptions) warnings
      goldenVsString
        (replaceExtension (getTestMain inputFiles) ".out")
        (return . T.encodeUtf8 . T.pack $ printDiagnosticsForGoldenTest fileContents result')

assertDoesNotCompile
  :: SupportModules
  -> [FilePath]
  -> [String]
  -> Expectation
assertDoesNotCompile support inputFiles shouldFailWith = do
  (fileContents, result) <- compile Nothing support inputFiles
  let errorOptions = P.defaultPPEOptions { P.ppeFileContents = fileContents }
  case fst result of
    Left errs -> do
      when (null shouldFailWith)
        (expectationFailure $
          "shouldFailWith declaration is missing (errors were: "
          ++ show (map P.errorCode (P.runMultipleErrors errs))
          ++ ")")
      checkShouldReport shouldFailWith (P.prettyPrintMultipleErrors errorOptions) errs
      goldenVsString
        (replaceExtension (getTestMain inputFiles) ".out")
        (return . T.encodeUtf8 . T.pack $ printDiagnosticsForGoldenTest fileContents result)
    Right _ ->
      expectationFailure "Should not have compiled"

assertCompilesToExpectedOutput
  :: SupportModules
  -> [FilePath]
  -> Expectation
assertCompilesToExpectedOutput support inputFiles = do
  (fileContents, (result, _)) <- compile Nothing support inputFiles
  let errorOptions = P.defaultPPEOptions { P.ppeFileContents = fileContents }
  case result of
    Left errs -> expectationFailure . P.prettyPrintMultipleErrors errorOptions $ errs
    Right _ ->
      goldenVsString
        (replaceExtension (getTestMain inputFiles) ".out.js")
        (BS.readFile $ modulesDir </> "Main/index.js")

-- Prints a set of diagnostics (i.e. errors or warnings) as a string, in order
-- to compare it to the contents of a golden test file.
printDiagnosticsForGoldenTest :: [(FilePath, T.Text)] -> (Either P.MultipleErrors a, P.MultipleErrors) -> String
printDiagnosticsForGoldenTest fileContents (result, warnings) =
  normalizePaths $ case result of
    Left errs ->
      -- TODO: should probably include warnings when failing?
      P.prettyPrintMultipleErrors errorOptions errs
    Right _ ->
      P.prettyPrintMultipleWarnings errorOptions warnings
  where
  errorOptions = P.defaultPPEOptions { P.ppeFileContents = fileContents }

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

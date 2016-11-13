{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

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

import Data.Char (isSpace)
import Data.Function (on)
import Data.List (sort, stripPrefix, intercalate, groupBy, sortBy, minimumBy)
import Data.Maybe (mapMaybe)
import Data.Time.Clock (UTCTime())
import Data.Tuple (swap)
import qualified Data.Text as T

import qualified Data.Map as M

import Control.Monad
import Control.Arrow ((***), (>>>))

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Trans.Except

import System.Exit
import System.Process hiding (cwd)
import System.FilePath
import System.Directory
import System.IO.UTF8
import System.IO.Silently
import qualified System.FilePath.Glob as Glob

import TestUtils
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  (supportExterns, passingTestCases, warningTestCases, failingTestCases) <- runIO $ do
    cwd <- getCurrentDirectory
    let passing = cwd </> "examples" </> "passing"
    let warning = cwd </> "examples" </> "warning"
    let failing = cwd </> "examples" </> "failing"
    let supportDir = cwd </> "tests" </> "support" </> "bower_components"
    let supportFiles ext = Glob.globDir1 (Glob.compile ("purescript-*/**/*." ++ ext)) supportDir
    passingFiles <- getTestFiles passing <$> testGlob passing
    warningFiles <- getTestFiles warning <$> testGlob warning
    failingFiles <- getTestFiles failing <$> testGlob failing
    supportPurs <- supportFiles "purs"
    supportPursFiles <- readInput supportPurs
    supportExterns <- runExceptT $ do
      modules <- ExceptT . return $ P.parseModulesFromFiles id (map (fmap T.pack) supportPursFiles)
      foreigns <- inferForeignModules modules
      externs <- ExceptT . fmap fst . runTest $ P.make (makeActions foreigns) (map snd modules)
      return (zip (map snd modules) externs)
    case supportExterns of
      Left errs -> fail (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)
      Right externs -> return (externs, passingFiles, warningFiles, failingFiles)

  context "Passing examples" $
    forM_ passingTestCases $ \testPurs ->
      it ("'" <> takeFileName (getTestMain testPurs) <> "' should compile and run without error") $
        assertCompiles supportExterns testPurs

  context "Warning examples" $
    forM_ warningTestCases $ \testPurs -> do
      let mainPath = getTestMain testPurs
      expectedWarnings <- runIO $ getShouldWarnWith mainPath
      it ("'" <> takeFileName mainPath <> "' should compile with warning(s) '" <> intercalate "', '" expectedWarnings <> "'") $
        assertCompilesWithWarnings supportExterns testPurs expectedWarnings

  context "Failing examples" $
    forM_ failingTestCases $ \testPurs -> do
      let mainPath = getTestMain testPurs
      expectedFailures <- runIO $ getShouldFailWith mainPath
      it ("'" <> takeFileName mainPath <> "' should fail with '" <> intercalate "', '" expectedFailures <> "'") $
        assertDoesNotCompile supportExterns testPurs expectedFailures

  where

  -- A glob for all purs and js files within a test directory
  testGlob :: FilePath -> IO [FilePath]
  testGlob = Glob.globDir1 (Glob.compile "**/*.purs")

  -- Groups the test files so that a top-level file can have dependencies in a
  -- subdirectory of the same name. The inner tuple contains a list of the
  -- .purs files and the .js files for the test case.
  getTestFiles :: FilePath -> [FilePath] -> [[FilePath]]
  getTestFiles baseDir
    = map (filter ((== ".purs") . takeExtensions) . map (baseDir </>))
    . groupBy ((==) `on` extractPrefix)
    . sortBy (compare `on` extractPrefix)
    . map (makeRelative baseDir)

  -- Takes the test entry point from a group of purs files - this is determined
  -- by the file with the shortest path name, as everything but the main file
  -- will be under a subdirectory.
  getTestMain :: [FilePath] -> FilePath
  getTestMain = minimumBy (compare `on` length)

  -- Extracts the filename part of a .purs file, or if the file is in a
  -- subdirectory, the first part of that directory path.
  extractPrefix :: FilePath -> FilePath
  extractPrefix fp =
    let dir = takeDirectory fp
        ext = reverse ".purs"
    in if dir == "."
       then maybe fp reverse $ stripPrefix ext $ reverse fp
       else dir

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

inferForeignModules
  :: MonadIO m
  => [(FilePath, P.Module)]
  -> m (M.Map P.ModuleName FilePath)
inferForeignModules = P.inferForeignModules . fromList
  where
    fromList :: [(FilePath, P.Module)] -> M.Map P.ModuleName (Either P.RebuildPolicy FilePath)
    fromList = M.fromList . map ((P.getModuleName *** Right) . swap)

trim :: String -> String
trim = dropWhile isSpace >>> reverse >>> dropWhile isSpace >>> reverse

modulesDir :: FilePath
modulesDir = ".test_modules" </> "node_modules"

makeActions :: M.Map P.ModuleName FilePath -> P.MakeActions P.Make
makeActions foreigns = (P.buildMakeActions modulesDir (P.internalError "makeActions: input file map was read.") foreigns False)
                         { P.getInputTimestamp = getInputTimestamp
                         , P.getOutputTimestamp = getOutputTimestamp
                         }
  where
  getInputTimestamp :: P.ModuleName -> P.Make (Either P.RebuildPolicy (Maybe UTCTime))
  getInputTimestamp mn
    | isSupportModule (P.runModuleName mn) = return (Left P.RebuildNever)
    | otherwise = return (Left P.RebuildAlways)
    where
    isSupportModule = flip elem supportModules

  getOutputTimestamp :: P.ModuleName -> P.Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    let filePath = modulesDir </> P.runModuleName mn
    exists <- liftIO $ doesDirectoryExist filePath
    return (if exists then Just (P.internalError "getOutputTimestamp: read timestamp") else Nothing)

readInput :: [FilePath] -> IO [(FilePath, String)]
readInput inputFiles = forM inputFiles $ \inputFile -> do
  text <- readUTF8File inputFile
  return (inputFile, text)

runTest :: P.Make a -> IO (Either P.MultipleErrors a, P.MultipleErrors)
runTest = P.runMake P.defaultOptions

compile
  :: [(P.Module, P.ExternsFile)]
  -> [FilePath]
  -> ([P.Module] -> IO ())
  -> IO (Either P.MultipleErrors [P.ExternsFile], P.MultipleErrors)
compile supportExterns inputFiles check = silence $ runTest $ do
  fs <- liftIO $ readInput inputFiles
  ms <- P.parseModulesFromFiles id (map (fmap T.pack) fs)
  foreigns <- inferForeignModules ms
  liftIO (check (map snd ms))
  let actions = makeActions foreigns
  case ms of
    [singleModule] -> pure <$> P.rebuildModule actions (map snd supportExterns) (snd singleModule)
    _ -> P.make actions (map fst supportExterns ++ map snd ms)

assert
  :: [(P.Module, P.ExternsFile)]
  -> [FilePath]
  -> ([P.Module] -> IO ())
  -> (Either P.MultipleErrors P.MultipleErrors -> IO (Maybe String))
  -> Expectation
assert supportExterns inputFiles check f = do
  (e, w) <- compile supportExterns inputFiles check
  maybeErr <- f (const w <$> e)
  maybe (return ()) expectationFailure maybeErr

checkMain :: [P.Module] -> IO ()
checkMain ms =
  unless (any ((== P.moduleNameFromString "Main") . P.getModuleName) ms)
    (fail "Main module missing")

checkShouldFailWith :: [String] -> P.MultipleErrors -> Maybe String
checkShouldFailWith expected errs =
  let actual = map P.errorCode $ P.runMultipleErrors errs
  in if sort expected == sort actual
    then Nothing
    else Just $ "Expected these errors: " ++ show expected ++ ", but got these: " ++ show actual

assertCompiles
  :: [(P.Module, P.ExternsFile)]
  -> [FilePath]
  -> Expectation
assertCompiles supportExterns inputFiles =
  assert supportExterns inputFiles checkMain $ \e ->
    case e of
      Left errs -> return . Just . P.prettyPrintMultipleErrors P.defaultPPEOptions $ errs
      Right _ -> do
        process <- findNodeProcess
        let entryPoint = modulesDir </> "index.js"
        writeFile entryPoint "require('Main').main()"
        result <- traverse (\node -> readProcessWithExitCode node [entryPoint] "") process
        case result of
          Just (ExitSuccess, out, err)
            | not (null err) -> return $ Just $ "Test wrote to stderr:\n\n" <> err
            | not (null out) && trim (last (lines out)) == "Done" -> return Nothing
            | otherwise -> return $ Just $ "Test did not finish with 'Done':\n\n" <> out
          Just (ExitFailure _, _, err) -> return $ Just err
          Nothing -> return $ Just "Couldn't find node.js executable"

assertCompilesWithWarnings
  :: [(P.Module, P.ExternsFile)]
  -> [FilePath]
  -> [String]
  -> Expectation
assertCompilesWithWarnings supportExterns inputFiles shouldWarnWith =
  assert supportExterns inputFiles checkMain $ \e ->
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
  :: [(P.Module, P.ExternsFile)]
  -> [FilePath]
  -> [String]
  -> Expectation
assertDoesNotCompile supportExterns inputFiles shouldFailWith =
  assert supportExterns inputFiles noPreCheck $ \e ->
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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Data.List (sort, stripPrefix, intercalate, groupBy, sortBy, partition)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Time.Clock (UTCTime())

import qualified Data.Map as M

import Control.Monad
import Control.Arrow ((>>>))

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

  (supportPurs, supportForeigns, passingTestCases, failingTestCases) <- runIO $ do
    cwd <- getCurrentDirectory
    let passing = cwd </> "examples" </> "passing"
    let failing = cwd </> "examples" </> "failing"
    let supportDir = cwd </> "tests" </> "support" </> "bower_components"
    let supportFiles ext = Glob.globDir1 (Glob.compile ("purescript-*/**/*." ++ ext)) supportDir
    passingFiles <- getTestFiles passing <$> testGlob passing
    failingFiles <- getTestFiles failing <$> testGlob failing
    supportPurs <- supportFiles "purs"
    supportForeigns <- loadForeigns =<< supportFiles "js"
    return (supportPurs, supportForeigns, passingFiles, failingFiles)

  context ("Passing examples") $ do
    forM_ passingTestCases $ \(testPurs, testJS) ->
      it ("'" <> takeFileName (getTestMain testPurs) <> "' should compile and run without error") $ do
        testForeigns <- loadForeigns testJS
        assertCompiles (supportPurs ++ testPurs) (supportForeigns <> testForeigns)

  context ("Failing examples") $ do
    forM_ failingTestCases $ \(testPurs, testJS) -> do
      let mainPath = getTestMain testPurs
      expectedFailures <- runIO $ getShouldFailWith mainPath
      it ("'" <> takeFileName mainPath <> "' should fail with '" <> intercalate "', '" expectedFailures <> "'") $ do
        testForeigns <- loadForeigns testJS
        assertDoesNotCompile (supportPurs ++ testPurs) (supportForeigns <> testForeigns) expectedFailures

  where

  -- A glob for all purs and js files within a test directory
  testGlob :: FilePath -> IO [FilePath]
  testGlob dir = join . fst <$> Glob.globDir (map Glob.compile ["**/*.purs", "**/*.js"]) dir

  -- Loads foreign modules from source files
  loadForeigns :: [FilePath] -> IO (M.Map P.ModuleName FilePath)
  loadForeigns paths = do
    files <- forM paths (\f -> (f,) <$> readUTF8File f)
    Right (foreigns, _) <- runExceptT $ runWriterT $ P.parseForeignModulesFromFiles files
    return foreigns

  -- Groups the test files so that a top-level file can have dependencies in a
  -- subdirectory of the same name. The inner tuple contains a list of the
  -- .purs files and the .js files for the test case.
  getTestFiles :: FilePath -> [FilePath] -> [([FilePath], [FilePath])]
  getTestFiles baseDir
    = map (partition ((== ".purs") . takeExtensions))
    . map (map (baseDir </>))
    . groupBy ((==) `on` extractPrefix)
    . sortBy (compare `on` extractPrefix)
    . map (makeRelative baseDir)

  -- Takes the test entry point from a group of purs files - this is determined
  -- by the file with the shortest path name, as everything but the main file
  -- will be under a subdirectory.
  getTestMain :: [FilePath] -> FilePath
  getTestMain = head . sortBy (compare `on` length)

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
  getShouldFailWith = fmap extractFailWiths . readUTF8File
    where
    extractFailWiths = lines >>> mapMaybe (stripPrefix "-- @shouldFailWith ") >>> map trim

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

type TestM = WriterT [(FilePath, String)] IO

runTest :: P.Make a -> IO (Either P.MultipleErrors a)
runTest = fmap fst . P.runMake P.defaultOptions

compile
  :: [FilePath]
  -> M.Map P.ModuleName FilePath
  -> IO (Either P.MultipleErrors P.Environment)
compile inputFiles foreigns = silence $ runTest $ do
  fs <- liftIO $ readInput inputFiles
  ms <- P.parseModuleHeadersFromFiles id fs
  let filePathMap = M.fromList $ map (\(fp, mh) -> (P.moduleHeaderName mh, fp)) ms
      loadModule name = do
        let path = fromMaybe (error "compile: no path for module name") $ M.lookup name filePathMap
            content = fromMaybe (error "compile: no content for module name") $ lookup path fs
        P.parseModuleFromFile path content
  P.make (makeActions foreigns) (map snd ms) loadModule

assert
  :: [FilePath]
  -> M.Map P.ModuleName FilePath
  -> (Either P.MultipleErrors P.Environment -> IO (Maybe String))
  -> Expectation
assert inputFiles foreigns f = do
  e <- compile inputFiles foreigns
  maybeErr <- f e
  maybe (return ()) expectationFailure maybeErr

assertCompiles :: [FilePath] -> M.Map P.ModuleName FilePath -> Expectation
assertCompiles inputFiles foreigns = do
  assert inputFiles foreigns $ \e ->
    case e of
      Left errs -> return . Just . P.prettyPrintMultipleErrors False $ errs
      Right _ -> do
        process <- findNodeProcess
        let entryPoint = modulesDir </> "index.js"
        writeFile entryPoint "require('Main').main()"
        result <- traverse (\node -> readProcessWithExitCode node [entryPoint] "") process
        case result of
          Just (ExitSuccess, out, err)
            | not (null err) -> return $ Just $ "Test wrote to stderr:\n\n" <> err
            | trim (last (lines out)) == "Done" -> return Nothing
            | otherwise -> return $ Just $ "Test did not finish with 'Done':\n\n" <> out
          Just (ExitFailure _, _, err) -> return $ Just err
          Nothing -> return $ Just "Couldn't find node.js executable"

assertDoesNotCompile
  :: [FilePath]
  -> M.Map P.ModuleName FilePath
  -> [String]
  -> Expectation
assertDoesNotCompile inputFiles foreigns shouldFailWith = do
  assert inputFiles foreigns $ \e ->
    case e of
      Left errs -> do
        return $ if null shouldFailWith
          then Just $ "shouldFailWith declaration is missing (errors were: "
                      ++ show (map P.errorCode (P.runMultipleErrors errs))
                      ++ ")"
          else checkShouldFailWith shouldFailWith errs
      Right _ ->
        return $ Just "Should not have compiled"

  where
  checkShouldFailWith expected errs =
    let actual = map P.errorCode $ P.runMultipleErrors errs
    in if sort expected == sort actual
      then Nothing
      else Just $ "Expected these errors: " ++ show expected ++ ", but got these: " ++ show actual

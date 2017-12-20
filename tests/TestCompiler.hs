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

import Data.Char (isSpace)
import Data.Function (on)
import Data.List (sort, stripPrefix, intercalate, groupBy, sortBy, minimumBy)
import Data.Maybe (mapMaybe)
import Data.Time.Clock (UTCTime())
import qualified Data.Text as T
import Data.Tuple (swap)

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
import System.IO
import System.IO.UTF8
import System.IO.Silently
import qualified System.FilePath.Glob as Glob

import TestUtils
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  (supportModules, supportExterns, supportForeigns, passingTestCases, warningTestCases, failingTestCases) <- runIO $ do
    cwd <- getCurrentDirectory
    let passing = cwd </> "examples" </> "passing"
    let warning = cwd </> "examples" </> "warning"
    let failing = cwd </> "examples" </> "failing"
    passingFiles <- getTestFiles passing <$> testGlob passing
    warningFiles <- getTestFiles warning <$> testGlob warning
    failingFiles <- getTestFiles failing <$> testGlob failing
    ms <- getSupportModuleTuples
    let modules = map snd ms
    supportExterns <- runExceptT $ do
      foreigns <- inferForeignModules ms
      externs <- ExceptT . fmap fst . runTest $ P.make (makeActions modules foreigns) modules
      return (externs, foreigns)
    case supportExterns of
      Left errs -> fail (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)
      Right (externs, foreigns) -> return (modules, externs, foreigns, passingFiles, warningFiles, failingFiles)

  outputFile <- runIO $ do
    tmp <- getTemporaryDirectory
    createDirectoryIfMissing False (tmp </> logpath)
    openFile (tmp </> logpath </> logfile) WriteMode

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

makeActions :: [P.Module] -> M.Map P.ModuleName FilePath -> P.MakeActions P.Make
makeActions modules foreigns = (P.buildMakeActions modulesDir (P.internalError "makeActions: input file map was read.") foreigns False)
                               { P.getInputTimestamp = getInputTimestamp
                               , P.getOutputTimestamp = getOutputTimestamp
                               }
  where
  getInputTimestamp :: P.ModuleName -> P.Make (Either P.RebuildPolicy (Maybe UTCTime))
  getInputTimestamp mn
    | isSupportModule (P.runModuleName mn) = return (Left P.RebuildNever)
    | otherwise = return (Left P.RebuildAlways)
    where
    isSupportModule = flip elem (map (P.runModuleName . P.getModuleName) modules)

  getOutputTimestamp :: P.ModuleName -> P.Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    let filePath = modulesDir </> T.unpack (P.runModuleName mn)
    exists <- liftIO $ doesDirectoryExist filePath
    return (if exists then Just (P.internalError "getOutputTimestamp: read timestamp") else Nothing)

runTest :: P.Make a -> IO (Either P.MultipleErrors a, P.MultipleErrors)
runTest = P.runMake P.defaultOptions

compile
  :: [P.Module]
  -> [P.ExternsFile]
  -> M.Map P.ModuleName FilePath
  -> [FilePath]
  -> ([P.Module] -> IO ())
  -> IO (Either P.MultipleErrors [P.ExternsFile], P.MultipleErrors)
compile supportModules supportExterns supportForeigns inputFiles check = silence $ runTest $ do
  fs <- liftIO $ readInput inputFiles
  ms <- P.parseModulesFromFiles id fs
  foreigns <- inferForeignModules ms
  liftIO (check (map snd ms))
  let actions = makeActions supportModules (foreigns `M.union` supportForeigns)
  case ms of
    [singleModule] -> pure <$> P.rebuildModule actions supportExterns (snd singleModule)
    _ -> P.make actions (supportModules ++ map snd ms)

assert
  :: [P.Module]
  -> [P.ExternsFile]
  -> M.Map P.ModuleName FilePath
  -> [FilePath]
  -> ([P.Module] -> IO ())
  -> (Either P.MultipleErrors P.MultipleErrors -> IO (Maybe String))
  -> Expectation
assert supportModules supportExterns supportForeigns inputFiles check f = do
  (e, w) <- compile supportModules supportExterns supportForeigns inputFiles check
  maybeErr <- f (const w <$> e)
  maybe (return ()) expectationFailure maybeErr

checkMain :: [P.Module] -> IO ()
checkMain ms =
  unless (any ((== P.moduleNameFromString "Main") . P.getModuleName) ms)
    (fail "Main module missing")

checkShouldFailWith :: [String] -> P.MultipleErrors -> Maybe String
checkShouldFailWith expected errs =
  let actual = map P.errorCode $ P.runMultipleErrors errs
  in if sort expected == sort (map T.unpack actual)
    then Nothing
    else Just $ "Expected these errors: " ++ show expected ++ ", but got these: " ++ show actual

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

logpath :: FilePath
logpath = "purescript-output"

logfile :: FilePath
logfile = "psc-tests.out"

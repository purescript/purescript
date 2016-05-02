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
import Data.Maybe (mapMaybe)
import Data.List (isSuffixOf, sort, stripPrefix, intercalate)
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
  (supportPurs, foreigns, passing, passingTestCases, failing, failingTestCases) <- runIO $ do
    cwd <- getCurrentDirectory

    let supportDir  = cwd </> "tests" </> "support" </> "bower_components"
    let supportFiles ext = Glob.globDir1 (Glob.compile ("purescript-*/**/*." ++ ext)) supportDir

    supportPurs <- supportFiles "purs"
    supportJS   <- supportFiles "js"

    foreignFiles <- forM supportJS (\f -> (f,) <$> readUTF8File f)
    Right (foreigns, _) <- runExceptT $ runWriterT $ P.parseForeignModulesFromFiles foreignFiles

    let passing = cwd </> "examples" </> "passing"
    passingTestCases <- sort . filter (".purs" `isSuffixOf`) <$> getDirectoryContents passing

    let failing = cwd </> "examples" </> "failing"
    failingTestCases <- sort . filter (".purs" `isSuffixOf`) <$> getDirectoryContents failing

    return (supportPurs, foreigns, passing, passingTestCases, failing, failingTestCases)

  context ("Passing examples") $ do
    forM_ passingTestCases $ \inputFile ->
      it ("'" <> inputFile <> "' should compile and run without error") $ do
        assertCompiles (supportPurs ++ [passing </> inputFile]) foreigns

  context ("Failing examples") $ do
    forM_ failingTestCases $ \inputFile -> do
      expectedFailures <- runIO $ getShouldFailWith (failing </> inputFile)
      it ("'" <> inputFile <> "' should fail with '" <> intercalate "', '" expectedFailures <> "'") $ do
        assertDoesNotCompile (supportPurs ++ [failing </> inputFile]) foreigns expectedFailures

  where

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
  ms <- P.parseModulesFromFiles id fs
  P.make (makeActions foreigns) (map snd ms)

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

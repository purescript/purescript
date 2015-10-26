-----------------------------------------------------------------------------
--
-- Module      :  Main
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

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

module Main (main) where

import qualified Language.PureScript as P
import qualified Language.PureScript.CodeGen.JS as J
import qualified Language.PureScript.CoreFn as CF

import Data.Char (isSpace)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (isSuffixOf, sort, stripPrefix)
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable (traverse)
#endif
import Data.Time.Clock (UTCTime())

import qualified Data.Map as M

import Control.Monad
import Control.Monad.IO.Class (liftIO)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Arrow ((>>>))

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Error.Class

import System.Exit
import System.Process
import System.FilePath
import System.Directory
import System.IO.UTF8
import qualified System.Info
import qualified System.FilePath.Glob as Glob

import Text.Parsec (ParseError)

import TestsSetup

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

compile :: [FilePath] -> M.Map P.ModuleName FilePath -> IO (Either P.MultipleErrors P.Environment)
compile inputFiles foreigns = runTest $ do
  fs <- liftIO $ readInput inputFiles
  ms <- P.parseModulesFromFiles id fs
  P.make (makeActions foreigns) (map snd ms)

assert :: [FilePath] ->
          M.Map P.ModuleName FilePath ->
          (Either P.MultipleErrors P.Environment -> IO (Maybe String)) ->
          TestM ()
assert inputFiles foreigns f = do
  e <- liftIO $ compile inputFiles foreigns
  maybeErr <- liftIO $ f e
  case maybeErr of
    Just err -> tell [(last inputFiles, err)]
    Nothing -> return ()

assertCompiles :: [FilePath] -> M.Map P.ModuleName FilePath -> TestM ()
assertCompiles inputFiles foreigns = do
  liftIO . putStrLn $ "Assert " ++ last inputFiles ++ " compiles successfully"
  assert inputFiles foreigns $ \e ->
    case e of
      Left errs -> return . Just . P.prettyPrintMultipleErrors False $ errs
      Right _ -> do
        process <- findNodeProcess
        let entryPoint = modulesDir </> "index.js"
        writeFile entryPoint "require('Main').main()"
        result <- traverse (\node -> readProcessWithExitCode node [entryPoint] "") process
        case result of
          Just (ExitSuccess, out, _) -> putStrLn out >> return Nothing
          Just (ExitFailure _, _, err) -> return $ Just err
          Nothing -> return $ Just "Couldn't find node.js executable"

assertDoesNotCompile :: [FilePath] -> M.Map P.ModuleName FilePath -> TestM ()
assertDoesNotCompile inputFiles foreigns = do
  let testFile = last inputFiles
  liftIO . putStrLn $ "Assert " ++ testFile ++ " does not compile"
  shouldFailWith <- getShouldFailWith testFile
  assert inputFiles foreigns $ \e ->
    case e of
      Left errs -> do
        putStrLn (P.prettyPrintMultipleErrors False errs)
        return $ if null shouldFailWith
          then Just $ "shouldFailWith declaration is missing (errors were: "
                      ++ show (map P.errorCode (P.runMultipleErrors errs))
                      ++ ")"
          else checkShouldFailWith shouldFailWith errs
      Right _ ->
        return $ Just "Should not have compiled"

  where
  getShouldFailWith =
    readUTF8File
    >>> liftIO
    >>> fmap (   lines
             >>> mapMaybe (stripPrefix "-- @shouldFailWith ")
             >>> map trim
             )

  checkShouldFailWith expected errs =
    let actual = map P.errorCode $ P.runMultipleErrors errs
    in if sort expected == sort actual
      then Nothing
      else Just $ "Expected these errors: " ++ show expected ++ ", but got these: " ++ show actual

  trim =
    dropWhile isSpace >>> reverse >>> dropWhile isSpace >>> reverse

main :: IO ()
main = do
  fetchSupportCode
  cwd <- getCurrentDirectory

  let supportDir  = cwd </> "tests" </> "support" </> "flattened"
  let supportFiles ext = Glob.globDir1 (Glob.compile ("*." ++ ext)) supportDir

  supportPurs <- supportFiles "purs"
  supportJS   <- supportFiles "js"

  foreignFiles <- forM supportJS (\f -> (f,) <$> readUTF8File f)
  Right (foreigns, _) <- runExceptT $ runWriterT $ P.parseForeignModulesFromFiles foreignFiles

  let passing = cwd </> "examples" </> "passing"
  passingTestCases <- sort . filter (".purs" `isSuffixOf`) <$> getDirectoryContents passing
  let failing = cwd </> "examples" </> "failing"
  failingTestCases <- sort . filter (".purs" `isSuffixOf`) <$> getDirectoryContents failing

  failures <- execWriterT $ do
    forM_ passingTestCases $ \inputFile ->
      assertCompiles (supportPurs ++ [passing </> inputFile]) foreigns
    forM_ failingTestCases $ \inputFile ->
      assertDoesNotCompile (supportPurs ++ [failing </> inputFile]) foreigns

  if null failures
    then exitSuccess
    else do
      putStrLn "Failures:"
      forM_ failures $ \(fp, err) ->
        let fp' = fromMaybe fp $ stripPrefix (failing ++ [pathSeparator]) fp
        in putStrLn $ fp' ++ ": " ++ err
      exitFailure

supportModules :: [String]
supportModules =
  [ "Control.Monad.Eff.Class"
  , "Control.Monad.Eff.Console"
  , "Control.Monad.Eff"
  , "Control.Monad.Eff.Unsafe"
  , "Control.Monad.ST"
  , "Data.Function"
  , "Prelude"
  , "Test.Assert"
  ]

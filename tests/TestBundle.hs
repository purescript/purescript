{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TestBundle where

import Prelude ()
import Prelude.Compat

import qualified Language.PureScript as P
import Language.PureScript.Bundle 

import Data.Function (on)
import Data.List (minimumBy)

import qualified Data.Map as M

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except

import System.Exit
import System.Process
import System.FilePath
import System.IO
import System.IO.UTF8
import qualified System.FilePath.Glob as Glob

import TestUtils
import Test.Tasty
import Test.Tasty.Hspec

main :: IO TestTree
main = testSpec "bundle" spec

spec :: Spec
spec = do
  (supportModules, supportExterns, supportForeigns) <- runIO $ setupSupportModules
  bundleTestCases <- runIO $ getTestFiles "bundle"
  outputFile <- runIO $ createOutputFile logfile 

  context "Bundle examples" $
    forM_ bundleTestCases $ \testPurs -> do
      it ("'" <> takeFileName (getTestMain testPurs) <> "' should compile, bundle and run without error") $
        assertBundles supportModules supportExterns supportForeigns testPurs outputFile
  where
  
  -- Takes the test entry point from a group of purs files - this is determined
  -- by the file with the shortest path name, as everything but the main file
  -- will be under a subdirectory.
  getTestMain :: [FilePath] -> FilePath
  getTestMain = minimumBy (compare `on` length)

assertBundles
  :: [P.Module]
  -> [P.ExternsFile]
  -> M.Map P.ModuleName FilePath
  -> [FilePath]
  -> Handle
  -> Expectation
assertBundles supportModules supportExterns supportForeigns inputFiles outputFile =
  assert supportModules supportExterns supportForeigns inputFiles checkMain $ \e ->
    case e of
      Left errs -> return . Just . P.prettyPrintMultipleErrors P.defaultPPEOptions $ errs
      Right _ -> do
        process <- findNodeProcess
        jsFiles <- Glob.globDir1 (Glob.compile "**/*.js") modulesDir
        let entryPoint = modulesDir </> "index.js"
        let entryModule = map (`ModuleIdentifier` Regular) ["Main"] 
        bundled <- runExceptT $ do
          input <- forM jsFiles $ \filename -> do
            js <- liftIO $ readUTF8File filename
            mid <- guessModuleIdentifier filename
            length js `seq` return (mid, Just filename, js) 
          bundleSM input entryModule (Just $ "Main") "PS" (Just entryPoint) Nothing
        case bundled of
            Right (_, js) -> do
              writeUTF8File entryPoint js
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
            Left err -> return . Just $ "Coud not bundle: " ++ show err

logfile :: FilePath
logfile = "bundle-tests.out"

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import System.Directory
import System.FilePath
import System.Process

import Criterion.Main

import Ide.Helper (runIde', defConfig)
import qualified Language.PureScript.Ide.Command as Command
import Language.PureScript.Ide.Types

inProject :: IO a -> IO a
inProject f = do
  cwd' <- getCurrentDirectory
  setCurrentDirectory ("benchmarks" </> "project")
  a <- f
  setCurrentDirectory cwd'
  pure a

compileProject :: IO Bool
compileProject = do
  (_, _, _, procHandle) <-
    createProcess (shell "psc-package build")
  r <- tryNTimes 20 (getProcessExitCode procHandle)
  pure (fromMaybe False (isSuccess <$> r))

isSuccess :: ExitCode -> Bool
isSuccess ExitSuccess = True
isSuccess (ExitFailure _) = False

tryNTimes :: Int -> IO (Maybe a) -> IO (Maybe a)
tryNTimes 0 _ = pure Nothing
tryNTimes n action = do
  r <- action
  case r of
    Nothing -> do
      threadDelay 500000
      tryNTimes (n - 1) action
    Just a -> pure (Just a)

loadWithoutGlobs :: IO (ModuleMap [IdeDeclarationAnn])
loadWithoutGlobs = do
   (results, st) <- runIde' (defConfig { confGlobs = [], confLogLevel = LogPerf }) emptyIdeState [Command.LoadSync []]
   pure (vsDeclarations (ideVolatileState st))

loadWithGlobs :: IO (ModuleMap [IdeDeclarationAnn])
loadWithGlobs = do
   (results, st) <-
     runIde' (defConfig { confGlobs = [ ".psc-package/psc-0.11.6-09272017/*/*/src/**/*.purs" ], confLogLevel = LogPerf })
     emptyIdeState [Command.LoadSync []]
   pure (vsDeclarations (ideVolatileState st))

-- Our benchmark harness.
main = inProject $ do
  hasCompiled <- compileProject
  if hasCompiled
    then
      defaultMain [
        bgroup "ide"
          [ bench "Without sourceglobs" $ nfIO loadWithoutGlobs
          , bench "With sourceglobs" $ nfIO loadWithGlobs
          ]]
    else
      panic "Couldn't compile the benchmark project"

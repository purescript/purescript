{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import System.Directory
import System.FilePath
import System.Process

import Criterion.Main

inProject :: IO a -> IO a
inProject f = do
  cwd' <- getCurrentDirectory
  print cwd'
  setCurrentDirectory ("benchmarks" </> "project")
  a <- f
  setCurrentDirectory cwd'
  pure a

compileProject :: IO Bool
compileProject = inProject $ do
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

-- Our benchmark harness.
main = do
  hasCompiled <- compileProject
  if hasCompiled
    then
      defaultMain [
        bgroup "ide" [ bench "Without sourceglobs" $ nfIO (pure ())
                     , bench "With sourceglobs" $ nfIO (pure ()) ]]
    else
      panic "Couldn't compile the benchmark project"

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import System.Directory
import System.FilePath
import System.Process
import System.Exit

import Criterion.Main

-- 1. Update all the packages
-- 2. Compile all the packages
-- 3. Load all files without source globs
-- 4. Load all files with source globs

-- The function we're benchmarking.
fib m | m < 0     = panic "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

updatePackages :: IO ()
updatePackages = do
  undefined

compilePackages :: IO ()
compilePackages = do
  undefined

inProject :: IO a -> IO a
inProject f = do
  cwd' <- getCurrentDirectory
  setCurrentDirectory ("." </> "benchmarks" </> "project")
  a <- f
  setCurrentDirectory cwd'
  pure a

compileTestProject :: IO Bool
compileTestProject = inProject $ do
  (_, _, _, procHandle) <-
    createProcess (shell "purs compile \"src/**/*.purs\"")
  r <- tryNTimes 10 (getProcessExitCode procHandle)
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
main = defaultMain [
  bgroup "fib" [ bench "1"  $ whnf fib 1
               , bench "5"  $ whnf fib 5
               , bench "9"  $ whnf fib 9
               , bench "11" $ whnf fib 11
               ]
  ]

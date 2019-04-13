#!/usr/bin/env stack
-- stack --resolver lts-13.12 script

-- This is the main CI build script. It is intended to run on all platforms we
-- run CI on: linux, mac os, and windows. It makes use of the following
-- environment variables:
--
-- BUILD_TYPE
-- May be one of the following:
--  - "normal": Compile & run tests normally
--  - "sdist": Create a source distribution and check that everything still
--    compiles and works
--  - "haddock": Check that haddock documentation builds correctly.
-- If unset, defaults to "normal".
--
-- CI_RELEASE
-- If set to "true", passes the RELEASE flag to the compiler and enables
-- optimizations.

{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (isJust, fromMaybe)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..), exitWith, exitFailure)
import System.Process (CreateProcess(..), proc, createProcess, waitForProcess, terminateProcess, getProcessExitCode, interruptProcessGroupOf)
import System.Timeout (timeout)

newtype Minutes = Minutes Int
  deriving (Show, Eq, Ord)

toMicroseconds :: Minutes -> Int
toMicroseconds (Minutes x) = x * microsecondsPerMinute
  where
  microsecondsPerMinute = 60 * (10^6)

data BuildStep = BuildStep
  { buildStepCommand :: String
  , buildStepArgs :: [String]
  , buildStepTimeout :: Maybe Minutes
    -- ^ If 'Just', this represents how long the process is permitted to run
    -- for.
  }
  deriving (Eq, Show)

command :: String -> BuildStep
command cmd = BuildStep
  { buildStepCommand = cmd
  , buildStepArgs = []
  , buildStepTimeout = Nothing
  }

displayCommand :: BuildStep -> String
displayCommand BuildStep{..} =
  intercalate " " (buildStepCommand : buildStepArgs)

stack :: [String] -> BuildStep
stack args =
  (command "stack")
    { buildStepArgs = ["--no-terminal", "--jobs=1"] ++ args
    }

data BuildType
  = Normal
  | Sdist
  | Haddock
  deriving (Eq, Show)

parseBuildType :: String -> Maybe BuildType
parseBuildType str =
  case map toLower str of
    "normal" ->
      Just Normal
    "sdist" ->
      Just Sdist
    "haddock" ->
      Just Haddock
    _ ->
      Nothing

data Options = Options
  { optsBuildType :: BuildType
  , optsCiRelease :: Bool
  }
  deriving (Eq, Show)

buildScript :: Options -> [BuildStep]
buildScript Options{..} =
  installStep : case optsBuildType of
    Normal ->
      [ buildAndTestStep
      ]
    Sdist ->
      [ stack ["sdist", "--test-tarball"]
      ]
    Haddock ->
      [ stack ["haddock", "--fast"]
      ]
  where
  optHaddock =
    if optsBuildType == Haddock then ["--haddock"] else []

  optRelease =
    if optsCiRelease then ["--flag=purescript:RELEASE"] else []

  optFast =
    if optsCiRelease then [] else ["--fast"]

  installStep =
    (stack
      (["build", "--only-dependencies", "--test"]
       ++ optHaddock ++ optFast))
      { buildStepTimeout = Just (Minutes 40)
      }

  buildAndTestStep =
    stack (["build", "--pedantic", "--test"] ++ optRelease ++ optFast)

getOptionsFromEnv :: IO Options
getOptionsFromEnv = do
  buildType <- fromMaybe "normal" <$> lookupEnv "BUILD_TYPE"
  optsBuildType <-
    case parseBuildType buildType of
      Just bt -> pure bt
      Nothing -> error $ "Unrecognised build type: '" ++ buildType ++ "'"

  ciRelease <- fromMaybe "" <$> lookupEnv "CI_RELEASE"
  let optsCiRelease =
        case map toLower ciRelease of
          "true" -> True
          _ -> False

  pure Options{..}

runBuildStep :: BuildStep -> IO ()
runBuildStep step@BuildStep{..} = do
  let p = proc buildStepCommand buildStepArgs

  putStrLn $ "> " ++ displayCommand step

  case buildStepTimeout of
    Just mins -> do
      (_, _, _, handle) <-
        createProcess $ p
          { create_group = True
          , use_process_jobs = True
          }
      r <- timeout (toMicroseconds mins) (waitForProcess handle)
      case r of
        Just exit ->
          checkExit exit
        Nothing -> do
          putStrLn ("Command timed out: " ++ displayCommand step)
          putStrLn "Try pushing a new commit to build again."
          cleanUp handle
          exitFailure
    Nothing -> do
      (_, _, _, handle) <- createProcess p
      waitForProcess handle >>= checkExit
 
  where
  checkExit ExitSuccess = pure ()
  checkExit f@(ExitFailure _) = exitWith f

  isDone handle = isJust <$> getProcessExitCode handle

  cleanUp handle = do
    interruptProcessGroupOf handle
    -- Give the process 10 seconds to finish cleanly, otherwise terminate it
    done <- checkDone 10 handle
    when (not done) (terminateProcess handle)

  checkDone remaining handle = do
    done <- isDone handle
    if done
      then pure True
      else
        if remaining <= 0
          then
            pure False
          else do
            threadDelay 1000
            checkDone (remaining - 1) handle

main :: IO ()
main = do
  opts <- getOptionsFromEnv
  putStrLn $ "Running with options: " ++ show opts
  mapM_ runBuildStep (buildScript opts)

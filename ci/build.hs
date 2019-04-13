#!/usr/bin/env stack
-- stack --resolver lts-13.12 script
-- Please use the same resolver above as is used in the compiler's stack.yaml file!

{-
This is the main CI build script. It is intended to run on all platforms we run
CI on: linux, mac os, and windows. It makes use of the following environment
variables:

- BUILD_TYPE

  May be one of the following:
   - "normal": Compile & run tests normally
   - "sdist": Create a source distribution and check that everything still
     compiles and works
   - "haddock": Check that haddock documentation builds correctly.

  If unset, the build type defaults to "normal".

- CI_RELEASE

  If set to "true", passes the RELEASE flag to the compiler, and enables
  optimizations. Otherwise, we disable optimizations (to speed builds up). 

# Stack & GHC

The script expects `stack setup` to have been run already; if this script is
run and the correct version GHC is not available, we can end up having stack
attempt to install all of the compiler's dependencies before the script
runs. (This is problematic because then the installation won't be subject to
the timeout mechanism inside the script).

# Timeouts

Unfortunately, we are forced to implement a command timeout mechanism for this
script. At the time of writing:

- Both CI platforms we use limit the length of time builds may take. Travis
  CI's limit is 50 minutes, and AppVeyor's is one hour.
- Setting up GHC and installing all of the compiler's dependencies takes longer
  than this limit allows.

Both Travis CI and AppVeyor provide a build cache mechanism, which allows you
to cache compiled artifacts in order to speed subsequent builds up. However,
when builds time out, we don't get the opportunity to upload a cache (since
we've already run out of time). Therefore, if we want the progress we have made
in a build to be saved to the build cache, we need to make sure we abort the
build early to allow time to upload the cache. Then, the next commit can pick
up where the previous commit left off.

## What to do when a build times out

If a CI build times out, you need to push a new commit. Amending and
force-pushing DOES NOT WORK. I suspect this is because CI platforms will only
consider a particular build cache to be appropriate to use when building a
given commit with if the cache was created by a parent of the commit being
built (which is sensible of them).
-}

{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Char (toLower, isSpace)
import Data.List (intercalate, dropWhileEnd)
import Data.Maybe (isJust, fromMaybe)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..), exitWith, exitFailure)
import System.Process (CreateProcess(..), proc, createProcess, readProcess, waitForProcess, terminateProcess, getProcessExitCode, interruptProcessGroupOf)
import System.Timeout (timeout)

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

newtype Minutes = Minutes Int
  deriving (Show, Eq, Ord)

toMicroseconds :: Minutes -> Int
toMicroseconds (Minutes x) = x * microsecondsPerMinute
  where
  microsecondsPerMinute = 60 * (10^6)

data BuildStep = BuildStep
  { buildStepCommand :: String
  , buildStepArgs :: [String]
  , buildStepWorkingDirectory :: Maybe String
  , buildStepTimeout :: Maybe Minutes
    -- ^ If 'Just', this represents how long the process is permitted to run
    -- for.
  }
  deriving (Eq, Show)

command :: String -> BuildStep
command cmd = BuildStep
  { buildStepCommand = cmd
  , buildStepArgs = []
  , buildStepWorkingDirectory = Nothing
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
  , optsVersion :: String
  -- ^ Version of the PureScript compiler. Used to determine the path that the
  -- source distribution will be written to.
  }
  deriving (Eq, Show)

buildScript :: Options -> [BuildStep]
buildScript Options{..} =
  installStep : case optsBuildType of
    Normal ->
      [ buildAndTestStep
      ]
    Sdist ->
      [ stack ["sdist", "--tar-dir", "sdist-test"]
      , (command "tar")
          { buildStepArgs =
              [ "-xzf" , "sdist-test/purescript-" ++ optsVersion ++ ".tar.gz"
              , "-C", "sdist-test"
              , "--strip-components=1"
              ]
          }
      , buildAndTestStep
          { buildStepWorkingDirectory = Just "sdist"
          }
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

getOptions :: IO Options
getOptions = do
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

  optsVersion <-
    trim <$> readProcess "stack" ["query", "locals", "purescript", "version"] ""

  pure Options{..}

runBuildStep :: BuildStep -> IO ()
runBuildStep step@BuildStep{..} = do
  let p = (proc buildStepCommand buildStepArgs)
            { cwd = buildStepWorkingDirectory
            }

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
  opts <- getOptions
  putStrLn $ "Running with options: " ++ show opts
  mapM_ runBuildStep (buildScript opts)

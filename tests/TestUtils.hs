{-# LANGUAGE ScopedTypeVariables #-}

module TestUtils where

import Prelude ()
import Prelude.Compat

import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Exception

import System.Process
import System.Directory
import System.Info

import Language.PureScript.Crash

findNodeProcess :: IO (Maybe String)
findNodeProcess = runMaybeT . msum $ map (MaybeT . findExecutable) names
  where
  names = ["nodejs", "node"]

-- |
-- Fetches code necessary to run the tests with. The resulting support code
-- should then be checked in, so that npm/bower etc is not required to run the
-- tests.
--
-- Simply rerun this (via ghci is probably easiest) when the support code needs
-- updating.
--
updateSupportCode :: IO ()
updateSupportCode = do
  setCurrentDirectory "tests/support"
  if System.Info.os == "mingw32"
    then callProcess "setup-win.cmd" []
    else do
      callProcess "npm" ["install"]
      -- Sometimes we run as a root (e.g. in simple docker containers)
      -- And we are non-interactive: https://github.com/bower/bower/issues/1162
      callProcess "node_modules/.bin/bower" ["--allow-root", "install", "--config.interactive=false"]
  setCurrentDirectory "../.."

pushd :: forall a. FilePath -> IO a -> IO a
pushd dir act = do
  original <- getCurrentDirectory
  setCurrentDirectory dir
  result <- try act :: IO (Either IOException a)
  setCurrentDirectory original
  either throwIO return result

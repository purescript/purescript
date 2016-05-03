{-# LANGUAGE ScopedTypeVariables #-}

module TestUtils where

import Prelude ()
import Prelude.Compat

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Exception

import System.Process
import System.Directory
import System.Info

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

-- |
-- The support modules that should be cached between test cases, to avoid
-- excessive rebuilding.
--
supportModules :: [String]
supportModules =
  [ "Control.Applicative"
  , "Control.Apply"
  , "Control.Bind"
  , "Control.Category"
  , "Control.Monad.Eff.Class"
  , "Control.Monad.Eff.Console"
  , "Control.Monad.Eff.Unsafe"
  , "Control.Monad.Eff"
  , "Control.Monad.ST"
  , "Control.Monad"
  , "Control.Semigroupoid"
  , "Data.Boolean"
  , "Data.BooleanAlgebra"
  , "Data.Bounded"
  , "Data.CommutativeRing"
  , "Data.Eq"
  , "Data.EuclideanRing"
  , "Data.Field"
  , "Data.Function.Uncurried"
  , "Data.Function"
  , "Data.Functor"
  , "Data.HeytingAlgebra"
  , "Data.Ord.Unsafe"
  , "Data.Ord"
  , "Data.Ordering"
  , "Data.Ring"
  , "Data.Semigroup"
  , "Data.Semiring"
  , "Data.Show"
  , "Data.Unit"
  , "Data.Void"
  , "Prelude"
  , "Test.Assert"
  ]

pushd :: forall a. FilePath -> IO a -> IO a
pushd dir act = do
  original <- getCurrentDirectory
  setCurrentDirectory dir
  result <- try act :: IO (Either IOException a)
  setCurrentDirectory original
  either throwIO return result

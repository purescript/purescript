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
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)

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
      -- bower uses shebang "/usr/bin/env node", but we might have nodejs
      node <- maybe cannotFindNode pure =<< findNodeProcess
      -- Sometimes we run as a root (e.g. in simple docker containers)
      -- And we are non-interactive: https://github.com/bower/bower/issues/1162
      callProcess node ["node_modules/.bin/bower", "--allow-root", "install", "--config.interactive=false"]
  setCurrentDirectory "../.."
  where
  cannotFindNode :: IO a
  cannotFindNode = do
    hPutStrLn stderr "Cannot find node (or nodejs) executable"
    exitFailure

-- |
-- The support modules that should be cached between test cases, to avoid
-- excessive rebuilding.
--
supportModules :: [String]
supportModules =
  [ "Control.Alt"
  , "Control.Alternative"
  , "Control.Applicative"
  , "Control.Apply"
  , "Control.Bind"
  , "Control.Category"
  , "Control.Comonad"
  , "Control.Extend"
  , "Control.Lazy"
  , "Control.Monad"
  , "Control.Monad.Eff"
  , "Control.Monad.Eff.Class"
  , "Control.Monad.Eff.Console"
  , "Control.Monad.Eff.Unsafe"
  , "Control.Monad.ST"
  , "Control.MonadPlus"
  , "Control.MonadZero"
  , "Control.Plus"
  , "Control.Semigroupoid"
  , "Data.Boolean"
  , "Data.BooleanAlgebra"
  , "Data.Bounded"
  , "Data.CommutativeRing"
  , "Data.Eq"
  , "Data.EuclideanRing"
  , "Data.Field"
  , "Data.Function"
  , "Data.Function.Uncurried"
  , "Data.Functor"
  , "Data.Functor.Invariant"
  , "Data.Generic.Rep"
  , "Data.Generic.Rep.Monoid"
  , "Data.Generic.Rep.Eq"
  , "Data.Generic.Rep.Ord"
  , "Data.Generic.Rep.Semigroup"
  , "Data.HeytingAlgebra"
  , "Data.Monoid"
  , "Data.Monoid.Additive"
  , "Data.Monoid.Conj"
  , "Data.Monoid.Disj"
  , "Data.Monoid.Dual"
  , "Data.Monoid.Endo"
  , "Data.Monoid.Multiplicative"
  , "Data.NaturalTransformation"
  , "Data.Newtype"
  , "Data.Ord"
  , "Data.Ord.Unsafe"
  , "Data.Ordering"
  , "Data.Ring"
  , "Data.Semigroup"
  , "Data.Semiring"
  , "Data.Symbol"
  , "Data.Show"
  , "Data.Unit"
  , "Data.Void"
  , "Partial"
  , "Partial.Unsafe"
  , "Prelude"
  , "Test.Assert"
  , "Test.Main"
  , "Unsafe.Coerce"
  ]

pushd :: forall a. FilePath -> IO a -> IO a
pushd dir act = do
  original <- getCurrentDirectory
  setCurrentDirectory dir
  result <- try act :: IO (Either IOException a)
  setCurrentDirectory original
  either throwIO return result

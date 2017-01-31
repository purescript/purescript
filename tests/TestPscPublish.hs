{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module TestPscPublish where

import Control.Monad.IO.Class (liftIO)
import System.Exit (exitFailure)
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (getCurrentTime)
import qualified Data.Aeson as A
import Data.Version

import Language.PureScript.Docs
import Language.PureScript.Publish
import Language.PureScript.Publish.ErrorsWarnings as Publish

import TestUtils

main :: IO ()
main = testPackage "tests/support/bower_components/purescript-prelude"

data TestResult
  = ParseFailed String
  | Mismatch ByteString ByteString -- ^ encoding before, encoding after
  | Pass ByteString
  deriving (Show)

roundTrip :: UploadedPackage -> TestResult
roundTrip pkg =
  let before = A.encode pkg
  in case A.eitherDecode before of
       Left err -> ParseFailed err
       Right parsed -> do
         let after = A.encode (parsed :: UploadedPackage)
         if before == after
           then Pass before
           else Mismatch before after

testRunOptions :: PublishOptions
testRunOptions = defaultPublishOptions
  { publishGetVersion = return testVersion
  , publishGetTagTime = const (liftIO getCurrentTime)
  , publishWorkingTreeDirty = return ()
  }
  where testVersion = ("v999.0.0", Version [999,0,0] [])

-- | Given a directory which contains a package, produce JSON from it, and then
-- | attempt to parse it again, and ensure that it doesn't change.
testPackage :: String -> IO ()
testPackage dir = pushd dir $ do
  res <- preparePackage testRunOptions
  case res of
    Left e -> preparePackageError e
    Right package -> case roundTrip package of
      Pass _ -> do
        putStrLn ("psc-publish test passed for: " ++ dir)
        pure ()
      other -> do
        putStrLn ("psc-publish tests failed on " ++ dir ++ ":")
        print other
        exitFailure
  where
    preparePackageError e = Publish.printErrorToStdout e >> exitFailure

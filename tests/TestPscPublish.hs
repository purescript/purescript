{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestPscPublish where

import Control.Monad
import Control.Applicative
import Control.Exception
import System.Process
import System.Directory
import System.IO
import System.Exit
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import qualified Data.Aeson as A
import Data.Aeson.BetterErrors
import Data.Version

import Language.PureScript.Docs
import Language.PureScript.Publish

pushd :: forall a. FilePath -> IO a -> IO a
pushd dir act = do
  original <- getCurrentDirectory
  setCurrentDirectory dir
  result <- try act :: IO (Either IOException a)
  setCurrentDirectory original
  either throwIO return result

data TestResult
  = ParseFailed String
  | Mismatch ByteString ByteString -- ^ encoding before, encoding after
  | Pass ByteString
  deriving (Show, Read)

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
  }
  where testVersion = ("v999.0.0", Version [999,0,0] [])

-- | Given a directory which contains a package, produce JSON from it, and then
-- | attempt to parse it again, and ensure that it doesn't change.
testPackage :: String -> IO ()
testPackage dir = do
  pushd dir $ do
    r <- roundTrip <$> preparePackage testRunOptions
    case r of
      Pass _ -> pure ()
      other -> do
        putStrLn ("psc-publish tests failed on " ++ dir ++ ":")
        putStrLn (show other)
        exitFailure

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | To run these tests:
--
-- * `cabal repl psc-publish`
-- * `:l psc-publish/tests/Test.hs`
-- * `test`

module Test where

import Control.Monad
import Control.Applicative
import System.Process
import System.Directory
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import qualified Data.Aeson as A
import Data.Aeson.BetterErrors

import Main
import Language.PureScript.Docs

packageName = "purescript-integers"
packageUrl = "https://github.com/purescript/" ++ packageName
packageDir = "tmp/" ++ packageName

pushd :: FilePath -> IO a -> IO a
pushd dir act = do
  original <- getCurrentDirectory
  setCurrentDirectory dir
  result <- act
  setCurrentDirectory original
  return result

clonePackage :: IO ()
clonePackage = do
  createDirectoryIfMissing True packageDir
  pushd packageDir $ do
    exists <- doesDirectoryExist ".git"
    unless exists $ do
      putStrLn ("Cloning " ++ packageName ++ " into " ++ packageDir ++ "...")
      readProcess "git" ["clone", packageUrl, "."] "" >>= putStr

bowerInstall :: IO ()
bowerInstall = do
  pushd packageDir $ do
    readProcess "bower" ["install"] "" >>= putStr

getPackage :: IO UploadedPackage
getPackage = do
  clonePackage
  bowerInstall
  pushd packageDir preparePackage

data TestResult
  = ParseFailed (ParseError UploadedPackageError)
  | Mismatch ByteString ByteString -- ^ encoding before, encoding after
  | Pass ByteString
  deriving (Show)

-- | Test JSON encoding/decoding; parse the package, roundtrip to/from JSON,
-- and check we get the same string.
test :: IO TestResult
test = roundTrip <$> getPackage

roundTrip :: UploadedPackage -> TestResult
roundTrip pkg =
  let before = A.encode pkg
  in case parse asUploadedPackage before of
       Left err -> ParseFailed err
       Right parsed -> do
         let after = A.encode parsed
         if before == after
           then Pass before
           else Mismatch before after

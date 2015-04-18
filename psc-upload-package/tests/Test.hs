{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test where

import Control.Monad
import Control.Applicative
import System.Process
import System.Directory
import Data.ByteString.Lazy (ByteString)
import qualified Data.Aeson as A
import Data.Aeson.BetterErrors

import Main
import Language.PureScript.Docs

preludeUrl = "https://github.com/purescript/purescript-prelude"
preludeDir = "tmp/purescript-prelude"

clonePrelude :: IO ()
clonePrelude =
  readProcess "git" ["clone", preludeUrl, preludeDir] "" >>= putStrLn

getPrelude :: IO UploadedPackage
getPrelude = do
  exists <- doesFileExist "bower.json"
  unless exists (setCurrentDirectory preludeDir)
  preparePackage

data TestResult
  = ParseFailed (ParseError UploadedPackageError)
  | Mismatch ByteString ByteString -- ^ encoding before, encoding after
  | Pass
  deriving (Show)

-- | Test JSON encoding/decoding; parse the prelude package, roundtrip
-- to/from JSON, and check we get the same string.
test :: IO TestResult
test = roundTrip <$> getPrelude

roundTrip :: UploadedPackage -> TestResult
roundTrip pkg =
  let before = A.encode pkg
  in case parse asUploadedPackage before of
       Left err -> ParseFailed err
       Right parsed -> do
         let after = A.encode parsed
         if (before == after)
           then Pass
           else Mismatch before after

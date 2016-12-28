{-# LANGUAGE TupleSections #-}
-- |
-- A small script which regenerates the LICENSE file with all
-- dependencies' licenses, when the dependencies are provided via standard
-- input.
--
-- It is recommended to run this as follows:
--
-- stack list-dependencies | stack exec runhaskell license-generator/generate.hs > LICENSE
--

module Main (main) where

import Control.Monad (forM_, when)
import Data.Char (isSpace, toLower)
import Data.Maybe (mapMaybe)
import Data.List
import Data.List.Split (splitOn)
import Data.Foldable
import Data.Traversable
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Network.HTTP.Types (ok200)
import Network.HTTP.Client (Manager, newManager, httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO (hPutStrLn, stderr, getContents)
import System.Exit (exitFailure)

main :: IO ()
main = do
  deps <- depsNamesAndVersions
  echoHeader
  putStrLn ""
  forM_ deps $ \(d, _) -> putStr "  " >> putStrLn d
  putStrLn ""
  manager <- newManager tlsManagerSettings
  results <- traverse (\d -> (d,) <$> depsLicense manager d) deps
  let failures = filter (not . snd) results
  if not (null failures)
    then do
      hPutStrLn stderr "Licenses were not found for the following packages:"
      traverse_ (hPutStrLn stderr . showPair . fst) failures
      exitFailure
    else
      hPutStrLn stderr "Everything looks ok."

  where
  showPair (pkg, version) = pkg ++ " " ++ version

echoHeader :: IO ()
echoHeader =
  readFile "license-generator/header.txt" >>= putStr

depsNamesAndVersions :: IO [(String, String)]
depsNamesAndVersions = do
  contents <- lines <$> getContents
  deps <- traverse parse contents
  pure (filter ((/= "purescript") . fst) deps)

  where
  parse line =
    case splitOn " " line of
      [pkg, vers] -> pure (pkg, vers)
      _ -> fail $ "Unable to parse input line: " ++ line

-- Returns True on success, False on failure.
depsLicense :: Manager -> (String, String) -> IO Bool
depsLicense manager dep = do
  hPutStrLn stderr (fst dep)
  result <- downloadLicenseFromHackage manager dep
  case result of
    FoundLicense license -> do
      putStrLn $ fst dep ++ " LICENSE file:"
      putStrLn ""
      putStrLn $ f license
      pure True
    LicenseNotNeeded ->
      pure True
    Failed ->
      pure False
  where
  f = unlines . map (trimEnd . ("  " ++)) . lines
  trimEnd = reverse . dropWhile isSpace . reverse

data LicenseResult
  = FoundLicense String
  | LicenseNotNeeded
  | Failed
  deriving (Show, Eq, Ord)

downloadLicenseFromHackage :: Manager -> (String, String) -> IO LicenseResult
downloadLicenseFromHackage manager dep = do
  mcabalFile <- downloadCabalFileFromHackage manager dep
  case mcabalFile of
    Nothing ->
      pure Failed
    Just cabalFile ->
      let
        field f = extractCabalField f cabalFile
      in
        case (field "license", field "license-file") of
          (_, Just licenseFile) -> do
            getLicense licenseFile
          (Just "PublicDomain", _) -> do
            pure LicenseNotNeeded
          _ -> do
            hPutStrLn stderr $
              "Unable to extract license information from cabal file for " ++
              fst dep
            pure Failed

  where
  getLicense licenseFile = do
    r <- downloadFromHackage ("/src/" ++ licenseFile) manager dep
    pure $ maybe Failed FoundLicense r

-- Attempt to extract a field from a cabal file. Note that this only works for
-- fields which are at the top level, not inside subsections such as
-- 'executable' or 'test-suite'.
extractCabalField :: String -> String -> Maybe String
extractCabalField fieldName cabalFile =
  case mapMaybe (stripPrefixCaseInsensitive fieldName) (lines cabalFile) of
    [line] ->
      Just $
        line
        |> dropWhile isSpace
        |> drop 1 -- colon
        |> trim
    _ ->
      Nothing
  where
  x |> f = f x

  trim =
    reverse . dropWhile isSpace . reverse . dropWhile isSpace

  stripPrefixCaseInsensitive prefix str =
    if map toLower prefix `isPrefixOf` map toLower str
      then Just (drop (length prefix) str)
      else Nothing

downloadCabalFileFromHackage :: Manager -> (String, String) -> IO (Maybe String)
downloadCabalFileFromHackage manager dep = do
  downloadFromHackage ("/src/" ++ fst dep ++ ".cabal") manager dep

downloadFromHackage :: String -> Manager -> (String, String) -> IO (Maybe String)
downloadFromHackage urlpath manager dep = do
  let url = hackageBaseUrl dep ++ urlpath
  req <- parseRequest url
  resp <- httpLbs req manager

  let status = responseStatus resp
  if (status /= ok200)
    then do
      hPutStrLn stderr $ "Bad status code for " ++ url
      hPutStrLn stderr $ "Expected 200, got " ++ show status
      pure Nothing
    else
      pure (Just (toString (responseBody resp)))

  where
  toString = TL.unpack . TLE.decodeUtf8

hackageBaseUrl :: (String, String) -> String
hackageBaseUrl (dep, version) =
  concat
    [ "https://hackage.haskell.org/package/"
    , dep
    , "-"
    , version
    ]

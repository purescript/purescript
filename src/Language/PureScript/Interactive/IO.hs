{-# LANGUAGE TypeApplications #-}

module Language.PureScript.Interactive.IO (findNodeProcess, readNodeProcessWithExitCode, getHistoryFilename) where

import Prelude

import Control.Monad (msum, void)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Functor ((<&>))
import Data.List (isInfixOf)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing,
                         getAppUserDataDirectory, getXdgDirectory,
                         findExecutable, doesFileExist)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcessWithExitCode)
import Text.Parsec ((<?>), many1, parse, sepBy)
import Text.Parsec.Char (char, digit)
import Protolude (note)

mkdirp :: FilePath -> IO ()
mkdirp = createDirectoryIfMissing True . takeDirectory

-- File helpers

onFirstFileMatching :: Monad m => (b -> m (Maybe a)) -> [b] -> m (Maybe a)
onFirstFileMatching f pathVariants = runMaybeT . msum $ map (MaybeT . f) pathVariants

-- |
-- Locates the node executable.
-- Checks for either @nodejs@ or @node@.
--
findNodeProcess :: IO (Either String String)
findNodeProcess = onFirstFileMatching findExecutable ["nodejs", "node"] <&>
  note "Could not find Node.js. Do you have Node.js installed and available in your PATH?"

findNodeVersion :: String -> IO (Maybe String)
findNodeVersion node = do
  result <- readProcessWithExitCode node ["--version"] ""
  return $ case result of
    (ExitSuccess, version, _) -> Just version
    (ExitFailure _, _, _) -> Nothing

readNodeProcessWithExitCode :: Maybe FilePath -> [String] -> String -> IO (Either String (ExitCode, String, String))
readNodeProcessWithExitCode nodePath nodeArgs stdin = runExceptT $ do
  process <- maybe (ExceptT findNodeProcess) pure nodePath
  (major, _, _) <- lift (findNodeVersion process) >>= \case
    Nothing -> throwError "Could not find Node.js version."
    Just version -> do
      let semver = do
            void $ char 'v'
            major : minor : patch : _ <- fmap (read @Int) (many1 digit) `sepBy` void (char '.')
            pure (major, minor, patch)
      case parse (semver <?> "Could not parse Node.js version.") "" version of
        Left err -> throwError $ show err
        Right (major, minor, patch)
          | major < 12 -> throwError $ "Unsupported Node.js version " <> show major <> ". Required Node.js version >=12."
          | otherwise -> pure (major, minor, patch)
  let nodeArgs' = if major < 13 then "--experimental-modules" : nodeArgs else nodeArgs
  lift (readProcessWithExitCode process nodeArgs' stdin) <&> \case
    (ExitSuccess, out, err) ->
      (ExitSuccess, out, censorExperimentalWarnings err)
    (ExitFailure code, out, err) ->
      (ExitFailure code, out, err)

censorExperimentalWarnings :: String -> String
censorExperimentalWarnings =
  unlines . filter (not . ("ExperimentalWarning" `isInfixOf`)) . lines

-- |
-- Grabs the filename where the history is stored.
--
getHistoryFilename :: IO FilePath
getHistoryFilename = do
  appuserdata <- getAppUserDataDirectory "purescript"
  olddirbool <- doesFileExist (appuserdata </> "psci_history")
  if olddirbool
      then return (appuserdata </> "psci_history")
      else do
        datadir <- getXdgDirectory XdgData "purescript"
        let filename = datadir </> "psci_history"
        mkdirp filename
        return filename

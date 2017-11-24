{-# LANGUAGE ScopedTypeVariables #-}

module TestUtils where

import PSPrelude

import qualified Language.PureScript as P

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List (sort)
import Data.String (String)
import qualified Data.Text as T
import System.Process
import System.Directory
import System.Info
import System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob

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
    fatal "Cannot find node (or nodejs) executable"

readInput :: [FilePath] -> IO [(FilePath, T.Text)]
readInput inputFiles = forM inputFiles $ \inputFile -> do
  text <- readFile inputFile
  return (inputFile, text)

-- |
-- The support modules that should be cached between test cases, to avoid
-- excessive rebuilding.
--
getSupportModuleTuples :: IO [(FilePath, P.Module)]
getSupportModuleTuples = do
  cd <- getCurrentDirectory
  let supportDir = cd </> "tests" </> "support" </> "bower_components"
  supportPurs <- Glob.globDir1 (Glob.compile "purescript-*/src/**/*.purs") supportDir
  supportPursFiles <- readInput supportPurs
  modules <- runExceptT $ ExceptT . return $ P.parseModulesFromFiles id supportPursFiles
  case modules of
    Right ms -> return ms
    Left errs -> failT (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)

getSupportModuleNames :: IO [T.Text]
getSupportModuleNames = sort . map (P.runModuleName . P.getModuleName . snd) <$> getSupportModuleTuples

pushd :: forall a. FilePath -> IO a -> IO a
pushd dir act = do
  original <- getCurrentDirectory
  setCurrentDirectory dir
  result <- try act :: IO (Either IOException a)
  setCurrentDirectory original
  either throwIO return result

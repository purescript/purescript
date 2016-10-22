module Language.PureScript.Interactive.IO (findNodeProcess, getHistoryFilename) where

import Prelude.Compat

import Control.Monad (msum)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import System.Directory (createDirectoryIfMissing, getHomeDirectory, findExecutable)
import System.FilePath (takeDirectory, (</>))

mkdirp :: FilePath -> IO ()
mkdirp = createDirectoryIfMissing True . takeDirectory

-- File helpers

onFirstFileMatching :: Monad m => (b -> m (Maybe a)) -> [b] -> m (Maybe a)
onFirstFileMatching f pathVariants = runMaybeT . msum $ map (MaybeT . f) pathVariants

-- |
-- Locates the node executable.
-- Checks for either @nodejs@ or @node@.
--
findNodeProcess :: IO (Maybe String)
findNodeProcess = onFirstFileMatching findExecutable names
  where names = ["nodejs", "node"]

-- |
-- Grabs the filename where the history is stored.
--
getHistoryFilename :: IO FilePath
getHistoryFilename = do
  home <- getHomeDirectory
  let filename = home </> ".purescript" </> "psci_history"
  mkdirp filename
  return filename

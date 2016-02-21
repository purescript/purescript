-----------------------------------------------------------------------------
--
-- Module      :  IO
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module PSCi.IO where

import Prelude ()
import Prelude.Compat

import System.Directory (createDirectoryIfMissing, getHomeDirectory, findExecutable, doesFileExist)
import System.FilePath (takeDirectory, (</>), isPathSeparator)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad (msum)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Console.Haskeline (outputStrLn, InputT)

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


-- |
-- Expands tilde in path.
--
expandTilde :: FilePath -> IO FilePath
expandTilde ('~':p:rest) | isPathSeparator p = (</> rest) <$> getHomeDirectory
expandTilde p = return p


whenFileExists :: MonadIO m => FilePath -> (FilePath -> InputT m ()) -> InputT m ()
whenFileExists filePath f = do
  absPath <- liftIO $ expandTilde filePath
  exists <- liftIO $ doesFileExist absPath
  if exists
    then f absPath
    else outputStrLn $ "Couldn't locate: " ++ filePath

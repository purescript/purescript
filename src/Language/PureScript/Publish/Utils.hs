
module Language.PureScript.Publish.Utils where

import Prelude.Compat

import Data.Either (partitionEithers)
import Data.List

import System.Directory
import System.Exit (exitFailure)
import System.FilePath (pathSeparator)
import System.IO (hPutStrLn, stderr)
import qualified System.FilePath.Glob as Glob

-- | Glob relative to the current directory, and produce relative pathnames.
globRelative :: Glob.Pattern -> IO [FilePath]
globRelative pat = do
  currentDir <- getCurrentDirectory
  glob pat currentDir

-- | Glob relative to the specified directory, and produce paths relative to the
-- current directory.
glob :: Glob.Pattern -> FilePath -> IO [FilePath]
glob pat target = do
  currentDir <- getCurrentDirectory
  filesAbsolute <- Glob.globDir1 pat target
  let prefix = currentDir ++ [pathSeparator]
  let (fails, paths) = partitionEithers . map (stripPrefix' prefix) $ filesAbsolute
  if null fails
    then return paths
    else do
      let p = hPutStrLn stderr
      p "Internal error in Language.PureScript.Publish.Utils.glob"
      p "Unmatched files:"
      mapM_ p fails
      exitFailure

  where
  stripPrefix' prefix dir =
    maybe (Left dir) Right $ stripPrefix prefix dir

-- | Glob pattern for PureScript source files.
purescriptSourceFiles :: Glob.Pattern
purescriptSourceFiles = Glob.compile "src/**/*.purs"

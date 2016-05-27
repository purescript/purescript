
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
  filesAbsolute <- Glob.globDir1 pat currentDir
  let prefix = currentDir ++ [pathSeparator]
  let (fails, paths) = partitionEithers . map (stripPrefix' prefix) $ filesAbsolute
  if null fails
    then return paths
    else do
      let p = hPutStrLn stderr
      p "Internal error in Language.PureScript.Publish.Utils.globRelative"
      p "Unmatched files:"
      mapM_ p fails
      exitFailure

  where
  stripPrefix' prefix dir =
    maybe (Left dir) Right $ stripPrefix prefix dir

-- | Glob pattern for PureScript source files.
purescriptSourceFiles :: Glob.Pattern
purescriptSourceFiles = Glob.compile "src/**/*.purs"

-- | Glob pattern for PureScript dependency files.
purescriptDepsFiles :: Glob.Pattern
purescriptDepsFiles = Glob.compile "bower_components/*/src/**/*.purs"

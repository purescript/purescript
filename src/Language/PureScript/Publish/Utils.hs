
module Language.PureScript.Publish.Utils where

import Data.List
import Data.Maybe
import System.Directory
import qualified System.FilePath.Glob as Glob

-- | Glob relative to the current directory, and produce relative pathnames.
globRelative :: Glob.Pattern -> IO [FilePath]
globRelative pat = do
  currentDir <- getCurrentDirectory
  filesAbsolute <- Glob.globDir1 pat currentDir
  return (mapMaybe (stripPrefix (currentDir ++ "/")) filesAbsolute)

-- | Glob pattern for PureScript source files.
purescriptSourceFiles :: Glob.Pattern
purescriptSourceFiles = Glob.compile "src/**/*.purs"

-- | Glob pattern for PureScript dependency files.
purescriptDepsFiles :: Glob.Pattern
purescriptDepsFiles = Glob.compile "bower_components/*/src/**/*.purs"

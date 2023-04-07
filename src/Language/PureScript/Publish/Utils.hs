module Language.PureScript.Publish.Utils where

import Prelude

import System.Directory (getCurrentDirectory)
import System.FilePath.Glob (Pattern, compile, globDir1)

-- | Glob relative to the current directory, and produce relative pathnames.
globRelative :: Pattern -> IO [FilePath]
globRelative pat = getCurrentDirectory >>= globDir1 pat

-- | Glob pattern for PureScript source files.
purescriptSourceFiles :: Pattern
purescriptSourceFiles = compile "src/**/*.purs"

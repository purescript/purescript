module SharedCLI where

import Prelude

import Options.Applicative qualified as Opts

inputFile :: Opts.Parser FilePath
inputFile = Opts.strArgument $
     Opts.metavar "GLOB"
  <> Opts.help "A glob for input .purs file(s)."

globInputFile :: Opts.Parser (Maybe FilePath)
globInputFile = Opts.optional $ Opts.strOption $
     Opts.long "source-globs-file"
  <> Opts.metavar "FILE"
  <> Opts.help "A file containing a line-separated list of input .purs globs."

excludeFiles :: Opts.Parser FilePath
excludeFiles = Opts.strOption $
     Opts.short 'x'
  <> Opts.long "exclude-files"
  <> Opts.metavar "GLOB"
  <> Opts.help "A glob of .purs files to exclude from the input .purs files."


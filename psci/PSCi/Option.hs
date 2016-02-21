module PSCi.Option (
  getOpt
) where

import Prelude ()
import Prelude.Compat

import Options.Applicative as Opts
import Data.Version (showVersion)

import PSCi.Types
import qualified Paths_purescript as Paths

-- Parse Command line option

multiLineMode :: Parser Bool
multiLineMode = switch $
     long "multi-line-mode"
  <> short 'm'
  <> Opts.help "Run in multi-line mode (use ^D to terminate commands)"

inputFile :: Parser FilePath
inputFile = strArgument $
     metavar "FILE"
  <> Opts.help "Optional .purs files to load on start"

inputForeignFile :: Parser FilePath
inputForeignFile = strOption $
     short 'f'
  <> long "ffi"
  <> help "The input .js file(s) providing foreign import implementations"

nodeFlagsFlag :: Parser [String]
nodeFlagsFlag = option parser $
     long "node-opts"
  <> metavar "NODE_OPTS"
  <> value []
  <> Opts.help "Flags to pass to node, separated by spaces"
  where
    parser = words <$> str

psciOptions :: Parser PSCiOptions
psciOptions = PSCiOptions <$> multiLineMode
                          <*> many inputFile
                          <*> many inputForeignFile
                          <*> nodeFlagsFlag

version :: Parser (a -> a)
version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> Opts.help "Show the version number" <> hidden

getOpt :: IO PSCiOptions
getOpt = execParser opts
    where
      opts        = info (version <*> helper <*> psciOptions) infoModList
      infoModList = fullDesc <> headerInfo <> footerInfo
      headerInfo  = header   "psci - Interactive mode for PureScript"
      footerInfo  = footer $ "psci " ++ showVersion Paths.version

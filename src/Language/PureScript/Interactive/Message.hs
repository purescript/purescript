module Language.PureScript.Interactive.Message where

import           Prelude.Compat

import           Data.List (intercalate)
import           Data.Version (showVersion)
import qualified Paths_purescript as Paths
import qualified Language.PureScript.Interactive.Directive as D
import           Language.PureScript.Interactive.Types

-- Messages

-- | The guide URL
guideURL :: String
guideURL = "https://github.com/purescript/documentation/blob/master/guides/PSCi.md"

-- | The help message.
helpMessage :: String
helpMessage = "The following commands are available:\n\n    " ++
  intercalate "\n    " (map line D.help) ++
  "\n\n" ++ extraHelp
  where
  line :: (Directive, String, String) -> String
  line (dir, arg, desc) =
    let cmd = ':' : D.stringFor dir
    in unwords [ cmd
               , replicate (11 - length cmd) ' '
               , arg
               , replicate (11 - length arg) ' '
               , desc
               ]

  extraHelp =
    "Further information is available on the PureScript documentation repository:\n" ++
    " --> " ++ guideURL

-- | The welcome prologue.
prologueMessage :: String
prologueMessage = unlines
  [ "PSCi, version " ++ showVersion Paths.version
  , "Type :? for help"
  ]

noInputMessage :: String
noInputMessage = unlines
  [ "purs repl: No input files; try running `pulp psci` instead."
  , "For help getting started, visit " ++ guideURL
  , "Usage: For basic information, try the `--help' option."
  ]

supportModuleMessage :: String
supportModuleMessage = unlines
  [ "purs repl: PSCi requires the psci-support package."
  , "For help getting started, visit " ++ guideURL
  ]

-- | The quit message.
quitMessage :: String
quitMessage = "See ya!"

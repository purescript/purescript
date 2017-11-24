module Language.PureScript.Interactive.Message where

import           PSPrelude

import           Data.Version (showVersion)
import qualified Paths_purescript as Paths
import qualified Language.PureScript.Interactive.Directive as D
import           Language.PureScript.Interactive.Types
import qualified Data.Text as T

-- Messages

-- | The guide URL
guideURL :: Text
guideURL = "https://github.com/purescript/documentation/blob/master/guides/PSCi.md"

-- | The help message.
helpMessage :: Text
helpMessage = "The following commands are available:\n\n    " <>
  T.intercalate "\n    " (map line D.help) <>
  "\n\n" <> extraHelp
  where
  line :: (Directive, Text, Text) -> Text
  line (dir, arg, desc) =
    let cmd = ":" <> D.stringFor dir
    in T.unwords [ cmd
                 , T.replicate (11 - T.length cmd) " "
                 , arg
                 , T.replicate (11 - T.length arg) " "
                 , desc
                 ]

  extraHelp =
    "Further information is available on the PureScript documentation repository:\n" <>
    " --> " <> guideURL

-- | The welcome prologue.
prologueMessage :: Text
prologueMessage = T.unlines
  [ "PSCi, version " <> toS (showVersion Paths.version)
  , "Type :? for help"
  ]

noInputMessage :: Text
noInputMessage = T.unlines
  [ "purs repl: No input files; try running `pulp psci` instead."
  , "For help getting started, visit " <> guideURL
  , "Usage: For basic information, try the `--help' option."
  ]

supportModuleMessage :: Text
supportModuleMessage = T.unlines
  [ "purs repl: PSCi requires the psci-support package."
  , "For help getting started, visit " <> guideURL
  ]

-- | The quit message.
quitMessage :: Text
quitMessage = "See ya!"

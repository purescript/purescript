module PSCi.Message where


import           Data.List (intercalate)
import           Data.Version (showVersion)
import qualified Paths_purescript as Paths
import qualified PSCi.Directive as D
import           PSCi.Types

-- Messages

-- |
-- The help message.
--
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
    "Further information is available on the PureScript wiki:\n" ++
    " --> https://github.com/purescript/purescript/wiki/psci"


-- |
-- The welcome prologue.
--
prologueMessage :: String
prologueMessage = unlines
  [ "PSCi, version " ++ showVersion Paths.version
  , "Type :? for help"
  ]

-- |
-- The quit message.
--
quitMessage :: String
quitMessage = "See ya!"

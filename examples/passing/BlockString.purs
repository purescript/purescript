module Main where

import Prelude

foo :: String
foo = """foo"""

main = Control.Monad.Eff.Console.log "Done"

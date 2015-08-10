module Main where

import Prelude

yield = 0
member = 1

public = \return -> return

this catch = catch

main = Control.Monad.Eff.Console.log "Done"

module Main where

import Prelude
import Control.Monad.Eff.Console (log)

yield = 0
member = 1

public = \return -> return

this catch = catch

main = log "Done"

-- @shouldFailWith ErrorParsingModule
module Bad_Module where

import Prelude
import Control.Monad.Eff.Console (log)

main = log "Done"

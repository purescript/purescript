-- @shouldFailWith ErrorParsingModule
module Bad_Module where

import Control.Monad.Eff.Console (log)

main = log "Done"

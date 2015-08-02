-- @shouldFailWith ErrorParsingModule
module Bad_Module where

import Prelude

main = Control.Monad.Eff.Console.log "Done"

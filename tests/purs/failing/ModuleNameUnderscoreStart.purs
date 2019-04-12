-- @shouldFailWith ErrorParsingModule
module _ModuleStartsWithUnderscore where

import Effect.Console (log)

main = log "Done"

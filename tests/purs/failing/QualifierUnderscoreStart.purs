-- @shouldFailWith ErrorParsingModule
module _QualifierStartsWithUnderscore.OkModuleName where

import Effect.Console (log)

main = log "Done"

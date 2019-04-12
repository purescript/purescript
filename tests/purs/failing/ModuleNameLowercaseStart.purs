-- @shouldFailWith ErrorParsingModule
module moduleNameStartsWithMinuscule where

import Effect.Console (log)

main = log "Done"

-- @shouldFailWith ErrorParsingModule
-- see #3601
module Bad'Module where

import Effect.Console (log)

main = log "Done"

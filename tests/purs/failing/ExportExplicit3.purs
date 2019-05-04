-- @shouldFailWith UnknownName
module Main where

import M1 as M
import Effect.Console (log)

-- should fail as Z is not exported from M1
testZ = M.Z

main = log "Done"

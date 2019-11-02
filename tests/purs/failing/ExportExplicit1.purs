-- @shouldFailWith UnknownName
module Main where

import M1
import Effect.Console (log)

testX = X

-- should fail as Y constructor is not exported from M1
testY = Y

main = log "Done"

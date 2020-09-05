-- @shouldFailWith UnknownName
-- @shouldFailWith UnknownName
-- should fail as X and Y constructors are not exported from M1
module Main where

import M1
import Effect.Console (log)

testX = X
testY = Y

main = log "Done"

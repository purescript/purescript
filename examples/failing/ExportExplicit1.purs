-- @shouldFailWith UnknownDataConstructor
module Main where

import Control.Monad.Eff.Console (log)
import M1

testX = X

-- should fail as Y constructor is not exported from M1
testY = Y

main = log "Done"

-- @shouldFailWith UnknownDataConstructor
module Main where

import M1

testX = X

-- should fail as Y constructor is not exported from M1
testY = Y

main = Control.Monad.Eff.Console.log "Done"

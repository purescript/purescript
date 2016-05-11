-- @shouldFailWith UnknownName
module Main where

import M1

-- should fail as Z is not exported from M1
testZ = M1.Z

main = Control.Monad.Eff.Console.log "Done"

-- @shouldFailWith UnknownDataConstructor
module Main where

import M1
import Control.Monad.Eff.Console (log)

-- should fail as Z is not exported from M1
testZ = M1.Z

main = log "Done"

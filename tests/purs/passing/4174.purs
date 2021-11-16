module Main where

import Data.Unit (Unit, unit)
import Effect.Console (log)

data Effect_Console = Effect_Console

d :: Effect_Console
d = Effect_Console

newtype Data_Unit = Data_Unit Unit

n :: Data_Unit
n = Data_Unit unit

main = log "Done"

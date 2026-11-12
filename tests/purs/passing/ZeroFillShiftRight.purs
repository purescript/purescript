module Main where

import Prelude

import Data.Int.Bits (zshr)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  assertEqual { actual: zshr (-8) 0, expected: -8 }
  assertEqual { actual: zshr (-8) 32, expected: -8 }
  assertEqual { actual: zshr (-8) (-32), expected: -8 }
  assertEqual { actual: zshr (-1) 1, expected: 2147483647 }
  assertEqual { actual: zshr bottom 1, expected: 1073741824 }
  assertEqual { actual: zshr (-8) 33, expected: 2147483644 }
  assertEqual { actual: zshr (-8) (-1), expected: 1 }
  assertEqual { actual: zshr 8 1, expected: 4 }
  log "Done"

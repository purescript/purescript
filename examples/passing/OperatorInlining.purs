module Main where

import Prelude
import Control.Monad.Eff.Console (logShow, log)

main = do

  -- semiringNumber
  logShow (1.0 + 2.0)
  logShow (1.0 * 2.0)

  -- ringNumber
  logShow (1.0 - 2.0)
  logShow (negate 1.0)

  -- moduleSemiringNumber
  logShow (1.0 / 2.0)

  -- ordNumber
  logShow (1.0 > 2.0)
  logShow (1.0 < 2.0)
  logShow (1.0 <= 2.0)
  logShow (1.0 >= 2.0)
  logShow (1.0 == 2.0)

  -- eqNumber
  logShow (1.0 == 2.0)
  logShow (1.0 /= 2.0)

  -- eqString
  logShow ("foo" == "bar")
  logShow ("foo" /= "bar")

  -- eqBoolean
  logShow (true == false)
  logShow (true /= false)

  -- semigroupString
  logShow ("foo" <> "bar")

  -- latticeBoolean
  logShow (top && true)
  logShow (bottom || false)

  -- complementedLatticeBoolean
  logShow (not true)

  log "Done"

module Main where

import Prelude
import Control.Monad.Eff.Console

main = do

  -- semiringNumber
  print (1.0 + 2.0)
  print (1.0 * 2.0)

  -- ringNumber
  print (1.0 - 2.0)
  print (negate 1.0)

  -- moduleSemiringNumber
  print (1.0 / 2.0)

  -- ordNumber
  print (1.0 > 2.0)
  print (1.0 < 2.0)
  print (1.0 <= 2.0)
  print (1.0 >= 2.0)
  print (1.0 == 2.0)

  -- eqNumber
  print (1.0 == 2.0)
  print (1.0 /= 2.0)

  -- eqString
  print ("foo" == "bar")
  print ("foo" /= "bar")

  -- eqBoolean
  print (true == false)
  print (true /= false)

  -- semigroupString
  print ("foo" ++ "bar")
  print ("foo" <> "bar")

  -- latticeBoolean
  print (top && true)
  print (bottom || false)

  -- complementedLatticeBoolean
  print (not true)

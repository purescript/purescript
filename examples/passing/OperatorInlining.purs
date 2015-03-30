module Main where

import Debug.Trace

main = do

  -- semiringNumber
  print (1 + 2)
  print (1 * 2)

  -- ringNumber
  print (1 - 2)
  print (negate 1)

  -- moduleSemiringNumber
  print (1 / 2)

  -- ordNumber
  print (1 > 2)
  print (1 < 2)
  print (1 <= 2)
  print (1 >= 2)
  print (1 == 2)

  -- eqNumber
  print (1 == 2)
  print (1 /= 2)

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

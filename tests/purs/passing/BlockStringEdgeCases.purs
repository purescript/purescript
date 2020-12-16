module Main where

import Prelude
import Effect.Console (log)
import Test.Assert (assert')

data Tuple a b = Tuple a b
derive instance tupleEq :: (Eq a, Eq b) => Eq (Tuple a b)

main = do
  assert' "empty string" ("""""" == "")
  assert' "quote" (""""""" == "\"")
  assert' "starts with quote" (""""x""" == "\"x")
  assert' "ends with quote" ("""x"""" == "x\"")
  assert' "two quotes" ("""""""" == "\"\"")
  assert' "starts with two quotes" ("""""x""" == "\"\"x")
  assert' "ends with two quotes" ("""x""""" == "x\"\"")
  assert' "starts and ends with two quotes" ("""""x""""" == "\"\"x\"\"")
  assert' "mixture 1" ("""""x"y""z"""" == "\"\"x\"y\"\"z\"")
  assert' "mixture 2" ("""x"y""z""" == "x\"y\"\"z")

  -- These last tests are more about forbidding certain raw string literal
  -- edge cases than about wanting to support mashing string literals against.
  -- each other, which is techically legal but generally, if not universally,
  -- a bad idea.
  assert' "too many quotes 1" (Tuple """"""""" " == Tuple "\"\"" " ")
  assert' "too many quotes 2" (Tuple """""""""" == Tuple "\"\"" "")
  assert' "too many quotes 3" (Tuple """x"""""" " == Tuple "x\"\"" " ")
  assert' "too many quotes 4" (Tuple """x""""""" == Tuple "x\"\"" "")
  log "Done"

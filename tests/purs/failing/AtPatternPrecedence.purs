-- See #3532
-- @shouldFailWith ArgListLengthsDiffer
module Main where

import Effect.Console (log)

data X = X String | Y

oops :: X -> String
-- previously this was parsed as x@(X s)
oops x@X s = s
oops Y = "Y"

main = log (oops (X "Done"))

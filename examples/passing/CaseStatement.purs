module Main where

import Prelude

data A = A | B | C

f a _ A = a
f _ a B = a
f _ _ C = "Done"

g a _ A = a
g _ b B = b
g _ _ C = C

data M a = N | J a

h f N a = a
h f a N = a
h f (J a) (J b) = J (f a b)

main = Debug.Trace.trace $ f "Done" "Failed" A

module Main where

import Effect.Console (log)

data X a = X (Y a) | Z
data Y a = Y (X a)

test1 = X (Y Z) :: X Int
test2 = X (Y Z) :: X "foo"
test3 = Y (X (Y Z)) :: Y Int
test4 = Y (X (Y Z)) :: Y "foo"

main = log "Done"

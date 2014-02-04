module TailCall where

import Prelude

test :: Number -> [Number] -> Number
test n [] = n
test n (x:xs) = test (n + x) xs

loop :: forall a. Number -> a
loop x = loop (x + 1)

notATailCall = \x -> 
  (\notATailCall -> notATailCall x) (\x -> x)
    
module Main where

main = Trace.trace "Done"

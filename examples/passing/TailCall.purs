module TailCall where

test n [] = n
test n (x:xs) = test (n + x) xs

loop x = loop (x + 1)

notATailCall = \x -> 
  (\notATailCall -> notATailCall x) (\x -> x)
    
module Main where

main = Trace.trace "Done"

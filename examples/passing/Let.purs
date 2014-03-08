module Main where

import Prelude
import Control.Monad.Eff

test1 x = let y = x + 1 in y

test2 x y = 
  let x' = x + 1 in
  let y' = y + 1 in
  x' + y'

test3 x = let 1 = x in 2

test4 = let f x y z = x + y + z in
        f 1 2 3

test5 = let f x [y, z] = x y z in 
        f (+) [1, 2]

main = do
  Debug.Trace.print (test1 1)
  Debug.Trace.print (test2 1 2)
  Debug.Trace.print (test3 1)
  Debug.Trace.print test4
  Debug.Trace.print test5

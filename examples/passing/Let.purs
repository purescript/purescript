module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST

test1 x = let
            y :: Number
            y = x + 1.0
          in y

test2 x y =
  let x' = x + 1.0 in
  let y' = y + 1.0 in
  x' + y'

test3 = let f x y z = x + y + z in
        f 1.0 2.0 3.0

test4 = let f x [y, z] = x y z in
        f (+) [1.0, 2.0]

test5 = let
          f x | x > 0.0 = g (x / 2.0) + 1.0
          f x = 0.0
          g x = f (x - 1.0) + 1.0
        in f 10.0

test6 = runPure (runST (do
          r <- newSTRef 0.0
          (let
            go [] = readSTRef r
            go (n : ns) = do
              modifySTRef r ((+) n)
              go ns
           in go [1.0, 2.0, 3.0, 4.0, 5.0])
        ))

test7 = let
          f :: forall a. a -> a
          f x = x
        in if f true then f 1.0 else f 2.0

test8 :: Number -> Number
test8 x = let
            go y | (x - 0.1 < y * y) && (y * y < x + 0.1) = y
            go y = go $ (y + x / y) / 2.0
          in go x

test10 _ =
  let
    f x = g x * 3.0
    g x = f x / 2.0
  in f 10.0

main = do
  Debug.Trace.print (test1 1.0)
  Debug.Trace.print (test2 1.0 2.0)
  Debug.Trace.print test3
  Debug.Trace.print test4
  Debug.Trace.print test5
  Debug.Trace.print test6
  Debug.Trace.print test7
  Debug.Trace.print (test8 100.0)

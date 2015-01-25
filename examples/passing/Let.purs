module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST

test1 x = let
            y :: Number
            y = x + 1
          in y

test2 x y =
  let x' = x + 1 in
  let y' = y + 1 in
  x' + y'

test3 = let f x y z = x + y + z in
        f 1 2 3

test4 = let f x [y, z] = x y z in
        f (+) [1, 2]

test5 = let
          f x | x > 0 = g (x / 2) + 1
          f x = 0
          g x = f (x - 1) + 1
        in f 10

test6 = runPure (runST (do
          r <- newSTRef 0
          (let
            go [] = readSTRef r
            go (n : ns) = do
              modifySTRef r ((+) n)
              go ns
           in go [1, 2, 3, 4, 5])
        ))

test7 = let
          f :: forall a. a -> a
          f x = x
        in if f true then f 1 else f 2

test8 :: Number -> Number
test8 x = let
            go y | (x - 0.1 < y * y) && (y * y < x + 0.1) = y
            go y = go $ (y + x / y) / 2
          in go x

test10 _ =
  let
    f x = g x * 3
    g x = f x / 2
  in f 10

main = do
  Debug.Trace.print (test1 1)
  Debug.Trace.print (test2 1 2)
  Debug.Trace.print test3
  Debug.Trace.print test4
  Debug.Trace.print test5
  Debug.Trace.print test6
  Debug.Trace.print test7
  Debug.Trace.print (test8 100)

module Main where

import Prelude.Unsafe (unsafeIndex)

test1 arr = arr `unsafeIndex` 0.0 + arr `unsafeIndex` 1.0 + 1.0

test2 = \arr -> case arr of
  [x, y] -> x + y
  [x] -> x
  [] -> 0.0
  (x : y : _) -> x + y

data Tree = One Number | Some [Tree]

test3 = \tree sum -> case tree of
  One n -> n
  Some (n1 : n2 : rest) -> test3 n1 sum * 10.0 + test3 n2 sum * 5.0 + sum rest

test4 = \arr -> case arr of
  [] -> 0.0
  [_] -> 0.0
  x : y : xs -> x * y + test4 xs

main = Debug.Trace.trace "Done"

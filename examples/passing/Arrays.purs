module Data.ArrayTests where

import Prelude

test1 arr = arr !! 0 + arr !! 1 + 1

test2 = \arr -> case arr of
  [x, y] -> x + y
  [x] -> x
  [] -> 0
  (x : y : _) -> x + y

data Tree = One Number | Some [Tree]

test3 = \tree sum -> case tree of
  One n -> n
  Some (n1 : n2 : rest) -> test3 n1 sum * 10 + test3 n2 sum * 5 + sum rest

test4 = \arr -> case arr of
  [] -> 0
  [_] -> 0
  x : y : xs -> x * y + test4 xs

module Main where

import Data.Array
import Prelude

main = do
  let x = [3,2,1]
  let y = sort x
  if x == [3,2,1]
    then Debug.Trace.trace "Done"
    else Control.Monad.Eff.Error.throwError "Not done"

module Main where

import Prelude
import Effect.Console (log)
import Test.Assert (assert)

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  derive (Functor)

main = do
  let t = Branch (Leaf 1) (Leaf 2)
  let t' = map (_ + 10) t
  assert $ case t' of
    Branch (Leaf a) (Leaf b) -> a == 11 && b == 12
    _ -> false
  log "Done"

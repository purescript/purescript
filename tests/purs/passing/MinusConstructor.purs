module Main where

import Prelude

import Effect.Console (log)
import Test.Assert (assert)

data Tuple a b = Tuple a b

infixl 6 Tuple as -

test1 =
  let tuple = "" - ""
      left - right = tuple
  in left

test2 = case 3 - 4 of
  left-4 -> left
  _ -> 0

test3 (Tuple a b - c) = a
test3 _ = 0

main = do
  assert $ test1 == ""
  assert $ test2 == 3
  assert $ test3 (5-10-15) == 5
  log "Done"

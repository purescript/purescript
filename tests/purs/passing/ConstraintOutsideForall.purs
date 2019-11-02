module Main where

import Effect.Console

class Test a

instance testUnit :: Test Int

test :: Test Int => forall a. a -> a
test a = a

main = log (test "Done")

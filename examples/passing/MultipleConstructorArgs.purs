module MultipleConstructorArgs where

import Arrays

data P a b = P a b

runP :: forall a b r. (a -> b -> r) -> P a b -> r
runP f (P a b) = f a b

idP = runP P

testCase = \p -> case p of
  P (x:xs) (y:ys) -> x : y : testCase (P xs ys)
  P _ _ -> []

test1 = testCase (P [1, 2, 3] [4, 5, 6])

module Main where

import Prelude
import MultipleConstructorArgs
import Global
import Eff
import Arrays

main = do
  Trace.trace (runP (\s n -> s ++ show n) (P "Test" 1))
  Trace.print test1

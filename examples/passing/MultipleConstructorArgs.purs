module Main where

import Prelude
import Control.Monad.Eff

data P a b = P a b

runP :: forall a b r. (a -> b -> r) -> P a b -> r
runP f (P a b) = f a b

idP = runP P

testCase = \p -> case p of
  P (x:xs) (y:ys) -> x + y
  P _ _ -> 0

test1 = testCase (P [1, 2, 3] [4, 5, 6])

main = do
  Debug.Trace.trace (runP (\s n -> s ++ show n) (P "Test" 1))
  Debug.Trace.print test1

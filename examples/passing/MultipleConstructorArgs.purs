module Main where

import Prelude
import Control.Monad.Eff

data P a b = P a b

runP :: forall a b r. (a -> b -> r) -> P a b -> r
runP f (P a b) = f a b

idP = runP P

testCase = \p -> case p of
  P (x:xs) (y:ys) -> x + y
  P _ _ -> 0.0

test1 = testCase (P [0.0, 0.0, 0.0] [0.0, 0.0, 0.0])

main = do
  Debug.Trace.trace (runP (\s n -> s ++ show n) (P "Test" 0.0))
  Debug.Trace.print test1

module Main where

import Control.Monad.Eff

match2 :: [Number] -> Number
match2 (x : y : xs) = x * y + match2 xs
match2 _ = 0.0

foreign import explode
  """
  function explode() {
    throw new Error('Incorrect result');
  }
  """ :: forall eff a. Eff eff a

main = case match2 [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0] of
  100.0 -> Debug.Trace.trace "Done"
  _ -> explode

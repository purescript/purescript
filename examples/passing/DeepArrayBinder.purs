module Main where

import Control.Monad.Eff

match2 :: [Number] -> Number
match2 (x : y : xs) = x * y + match2 xs
match2 _ = 0

foreign import explode
  """
  function explode() {
    throw new Error('Incorrect result');
  }
  """ :: forall eff a. Eff eff a

main = case match2 [1, 2, 3, 4, 5, 6, 7, 8, 9] of
  100 -> Debug.Trace.trace "Done"
  _ -> explode

module Main where

import Control.Monad.Eff
import Debug.Trace

foreign import f
  """
  function f(x) {
    return function() {
      if (x !== 0) throw new Error('x is not 0');
    };
  }
  """ :: forall e. Number -> Eff e Number

main = do
  let x = 0.0
  f x
  let x = 1.0 + 1.0
  trace "Done"

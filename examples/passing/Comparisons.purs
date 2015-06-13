module Main where

import Control.Monad.Eff
import Debug.Trace

foreign import data Assert :: !

foreign import assert
  """
  function assert(x) {
    return function() {
      if (!x) throw new Error('assertion failed');
      return {};
    };
  }
  """ :: forall e. Boolean -> Eff (assert :: Assert | e) Unit

main = do
  assert (1.0 < 2.0)
  assert (2.0 == 2.0)
  assert (3.0 > 1.0)
  assert ("a" < "b")
  assert ("a" == "a")
  assert ("z" > "a")
  trace "Done!"

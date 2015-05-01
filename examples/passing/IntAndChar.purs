module Main where

import Control.Monad.Eff

f 1 = 1
f _ = 0

g 'a' = 'a'
g _ = 'b'

foreign import assertEqual
  """
  function assertEqual(a) { return function(b) { return function() {
    if (a != b) {
      throw new Error('Assertion failed!');
    }
  }; }; }
  """ :: forall eff a. a -> a -> Eff eff Unit

main = do
  assertEqual (f 1) 1 
  assertEqual (f 0) 0
  assertEqual (g 'a') 'a'
  assertEqual (g 'b') 'b'
  Debug.Trace.trace "Done"

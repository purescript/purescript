module Main where

import Control.Monad.Eff
import Debug.Trace

foreign import data Assert :: !

foreign import assert
  """
  function assert(x) {
    return function(desc) {
      return function() {
        if (!x) throw new Error('assertion (' + desc + ') failed');
        return {};
      };
    };
  }
  """ :: forall e. Boolean -> String -> Eff (assert :: Assert | e) Unit

bug :: Number -> Number -> Number
bug a b = 0 - (a - b)

foreign import explode
  """
  function explode() {
    throw new Error('Assertion failed!');
  }
  """ :: forall eff a. Eff eff a

main = do
    assert (bug 0 2 == 2)       "bug 0 2 == 2"
    assert (0 - (0 - 2) == 2)   "0 - (0 - 2) == 2"
    assert (0 - (0 + 2) == -2)  "0 - (0 + 2) == -2"

    assert (6 / (3 * 2) == 1)   "6 / (3 * 2) == 1"
    assert ((6 / 3) * 2 == 4)   "(6 / 3) * 2 == 4"

    assert (6 % (2 * 2) == 2)   "6 % (2 * 2) == 2"
    assert ((6 % 2) * 2 == 0)   "(6 % 2) * 2 == 0"

    assert (4 % (9 / 3) == 1)   "4 % (9 / 3) == 1"
    assert ((4 % 9) / 2 == 2)   "(4 % 9) / 2 == 2"

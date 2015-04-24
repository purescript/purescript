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
bug a b = 0.0 - (a - b)

foreign import explode
  """
  function explode() {
    throw new Error('Assertion failed!');
  }
  """ :: forall eff a. Eff eff a

main = do
    assert (bug 0.0 2.0 == 2.0)       "bug 0.0 2.0 == 2.0"
    assert (0.0 - (0.0 - 2.0) == 2.0)   "0.0 - (0.0 - 2.0) == 2.0"
    assert (0.0 - (0.0 + 2.0) == -2.0)  "0.0 - (0.0 + 2.0) == -2.0"

    assert (6.0 / (3.0 * 2.0) == 1.0)   "6.0 / (3.0 * 2.0) == 1.0"
    assert ((6.0 / 3.0) * 2.0 == 4.0)   "(6.0 / 3.0) * 2.0 == 4.0"

    assert (not (1.0 < 0.0) == true) "not (1.0 < 0.0) == true"
    assert (not ((negate 1.0) < 0.0) == false) "not ((negate 1.0) < 0.0) == false"

    assert (negate (1.0 + 10.0) == -11.0) "negate (1.0 + 10.0) == -11.0"

    assert (2.0 * 3.0 / 4.0 == 1.5) "2.0 * 3.0 / 4.0 == 1.5"
    assert (1.0 * 2.0 * 3.0 * 4.0 * 5.0 / 6.0 == 20.0) "1.0 * 2.0 * 3.0 * 4.0 * 5.0 / 6.0 == 20.0"

    assert (1.0 + 10.0 - 5.0 == 6.0) "1.0 + 10.0 - 5.0 == 6.0"
    assert (1.0 + 10.0 * 5.0 == 51.0) "1.0 + 10.0 * 5.0 == 51.0"
    assert (10.0 * 5.0 - 1.0 == 49.0) "10.0 * 5.0 - 1.0 == 49.0"

    trace "Success!"

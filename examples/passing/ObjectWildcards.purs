module Main where

import Control.Monad.Eff
import Debug.Trace

mkRecord = { foo: _, bar: _, baz: "baz" }

getValue :: forall e. Eff (| e) Boolean
getValue = return true

foreign import eqeqeq
  """
  function eqeqeq(x) {
    return function (y) {
      if (x == y) return x;
      throw new Error("Unexpected result: " + x + " /== " + y);
    };
  };
  """ :: forall a. a -> a -> a

(===) = eqeqeq
infixl 4 ===

main = do
  obj <- { value: _ } <$> getValue
  print obj.value
  let x = 1
  point <- { x: _, y: x } <$> return 2
  print $ point.x === 2
  print $ point.y === 1
  trace (mkRecord 1 "Done!").bar

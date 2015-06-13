module Main where

import Control.Monad.Eff
import Debug.Trace

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

getValue :: forall e. Eff (| e) Boolean
getValue = return true

main = do
  let record = { value: false }
  record' <- record { value = _ } <$> getValue
  print $ record'.value === true

  let point = { x: 1.0, y: 1.0 }
      x = 10.0
      point' = (point { x = _, y = x }) 100.0

  print $ point'.x === 100.0
  print $ point'.y === 10.0

  let record2 = (_ { x = _ }) { x: 0.0 } 10.0
  print $ record2.x === 10.0

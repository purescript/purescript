module Main where

import Prelude

data Z
data S n

data ArrayBox n a = ArrayBox [a]

nil :: forall a. ArrayBox Z a
nil = ArrayBox []

foreign import concat
  """
  function concat(l1) {
    return function(l2) {
      return l1.concat(l2);
    };
  }
  """ :: forall a. [a] -> [a] -> [a]

cons' :: forall a n. a -> ArrayBox n a -> ArrayBox (S n) a
cons' x (ArrayBox xs) = ArrayBox $ concat [x] xs

foreign import error
  """
  function error(msg) {
    throw msg;
  }
  """ :: forall a. String -> a

main = case cons' 1 $ cons' 2 $ cons' 3 nil of
         ArrayBox [1, 2, 3] -> Debug.Trace.trace "Done"
         _ -> error "Failed"

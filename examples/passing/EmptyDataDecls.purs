module Main where

import Prelude

data Z
data S n

data Array n a = Array [a]

nil :: forall a. Array Z a
nil = Array []

foreign import concat
  "function concat(l1) {\
  \  return function (l2) {\
  \    return l1.concat(l2);\
  \  };\
  \}" :: forall a. [a] -> [a] -> [a]

cons' :: forall a n. a -> Array n a -> Array (S n) a
cons' x (Array xs) = Array $ concat [x] xs

foreign import error
    "function error(msg) {\
    \  throw msg;\
    \}" :: forall a. String -> a

main = case cons' 1 $ cons' 2 $ cons' 3 nil of
         Array [1, 2, 3] -> Debug.Trace.trace "Done"
         _ -> error "Failed"

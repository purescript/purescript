module Main where

import Prelude
import Test.Assert

data Z
data S n

data ArrayBox n a = ArrayBox (Array a)

nil :: forall a. ArrayBox Z a
nil = ArrayBox []

cons' :: forall a n. a -> ArrayBox n a -> ArrayBox (S n) a
cons' x (ArrayBox xs) = ArrayBox $ append [x] xs

main = case cons' 1 $ cons' 2 $ cons' 3 nil of
         ArrayBox [1, 2, 3] -> Control.Monad.Eff.Console.log "Done"
         _ -> assert' "Failed" false

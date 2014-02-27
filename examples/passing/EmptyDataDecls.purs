module Main where

import Prelude
import Data.Array

data Z
data S n

data Array n a = Array [a]

nil :: forall a. Array Z a
nil = Array []

cons :: forall a n. a -> Array n a -> Array (S n) a
cons x (Array xs) = Array $ x : xs

main = let (Array xs) = cons 1 $ cons 2 $ cons 3 nil
       in if xs == [1, 2, 3] 
          then Debug.Trace.trace "Done"
          else Control.Monad.Error.throwError "Failed"

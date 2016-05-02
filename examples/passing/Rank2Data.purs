module Main where

import Prelude hiding (add)
import Control.Monad.Eff.Console (log)

data Id = Id forall a. a -> a

runId = \id a -> case id of
  Id f -> f a

data Nat = Nat forall r. r -> (r -> r) -> r

runNat = \nat -> case nat of
  Nat f -> f 0.0 (\n -> n + 1.0)

zero' = Nat (\zero' _ -> zero')

succ = \n -> case n of
  Nat f -> Nat (\zero' succ -> succ (f zero' succ))

add = \n m -> case n of
  Nat f -> case m of
    Nat g -> Nat (\zero' succ -> g (f zero' succ) succ)

one' = succ zero'
two = succ zero'
four = add two two
fourNumber = runNat four

main = log "Done"

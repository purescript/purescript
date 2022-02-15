module Main where

import Effect.Console

data Tuple a b = Tuple a b

v2 :: forall @a @b. a -> b -> Tuple a b
v2 = Tuple

v2' :: Tuple Int Int
v2' = v2 @Int @Int 21 42

main = log "Done"

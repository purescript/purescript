module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

type PolyR = { id :: forall a. a -> a }

test :: PolyR
test = { id: \a -> a }

wat :: (forall b. b -> b) -> Unit
wat _ =  unit
  
example :: Unit
example = let { id } = test in wat id

example1 :: PolyR -> Unit
example1 { id } = wat id

type Stuff =
  { foo :: forall a. a -> a
  , bar :: forall a. Eq a => a -> a
  }

broken :: Stuff -> Int
broken { foo, bar } =
  bar $ foo 1

main :: Effect Unit
main = log "Done"

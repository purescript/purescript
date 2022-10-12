module Main where

import Effect.Console (log)

test0 = ((((\_ -> 0) :: b -> Int) :: forall b. b -> Int) :: forall a. a -> Int)

test1 :: {attr :: forall a. a -> Int}
test1 = {attr: ((\_ -> 0) :: b -> Int) :: forall b. b -> Int}

class Test2 where
  f :: forall a. a -> a

instance test2 :: Test2 where
  f :: forall a. a -> a
  f x = (x :: a)


main = log "Done"

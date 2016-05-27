module Main where

import Prelude
import Control.Monad.Eff.Console (log)

test1 :: forall a. (a -> a) -> a -> a
test1 f x = g (g x)
  where
  g :: a -> a
  g y = f (f y)

test2 :: forall a. (a -> a) -> a -> a
test2 = h
  where
  h :: forall b. (b -> b) -> b -> b
  h f x = g (g x)
    where
    g :: b -> b
    g y = f (f y)

test3 :: Number
test3 = ((\b -> b :: b) :: forall b. b -> b) 0.0

test4 :: forall a. (a -> a) -> a -> a
test4 = h
  where
  h :: forall b. (b -> b) -> b -> b
  h f x = g (g x)
    where
    g :: b -> b
    g y = j (f (f y))
      where
      j :: forall b. b -> b
      j x = x


main = log "Done"

module Main where

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
test3 = ((\b -> b :: b) :: forall b. b -> b) 0

main = Debug.Trace.trace "Done"

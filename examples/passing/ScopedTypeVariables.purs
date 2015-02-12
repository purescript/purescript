module Main where

test1 :: forall a. (a -> a) -> a -> a
test1 f x = g (g x)
  where
  g :: a -> a
  g y = f (f y)

main = Debug.Trace.trace "Done"

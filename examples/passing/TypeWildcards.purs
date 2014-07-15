module Main where

test :: forall a. (Eq a) => (a -> a) -> a -> a
test f a = go (f a) a
  where
  go :: _ -> _ -> _
  go a1 a2 | a1 == a2 = a1
  go a1 _ = go (f a1) a1

main = Debug.Trace.trace "Done"

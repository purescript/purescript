module Main where

zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _  = []
zipWith _ _  [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

test1 = [1, 2, 3] `zipWith (+)` [4, 5, 6]

comparing :: forall a b. (Ord b) => (a -> b) -> a -> a -> Ordering
comparing f = compare `Data.Function.on` f

sum [] = 0
sum (x:xs) = x + sum xs

test2 = [1, 2, 3] `comparing sum` [4, 5, 6]

main = do
  Debug.Trace.print test1
  Debug.Trace.print test2

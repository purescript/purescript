module Main where

zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _  = []
zipWith _ _  [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

test1 = [1.0, 2.0, 3.0] `zipWith (+)` [4.0, 5.0, 6.0]

comparing :: forall a b. (Ord b) => (a -> b) -> a -> a -> Ordering
comparing f = compare `Data.Function.on` f

sum [] = 0.0
sum (x:xs) = x + sum xs

test2 = [1.0, 2.0, 3.0] `comparing sum` [4.0, 5.0, 6.0]

main = do
  Debug.Trace.print test1
  Debug.Trace.print test2

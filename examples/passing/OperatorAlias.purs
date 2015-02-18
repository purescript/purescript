module Main where

data Maybe a = Nothing | Just a

infixl 4 ? as orElse

(?) :: forall a. Maybe a -> a -> a
(?) Nothing a = a
(?) (Just a) _ = a

test1 x = x ? 0

test2 x = x ? 0
  where
  (?) _ y = y

main = Debug.Trace.trace "Done"

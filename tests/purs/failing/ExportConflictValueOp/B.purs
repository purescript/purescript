module B where

f2 :: forall a b. a -> b -> a
f2 x _ = x

infix 0 f2 as !!

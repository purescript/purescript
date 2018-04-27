module A where

f1 :: forall a b. a -> b -> a
f1 x _ = x

infix 0 f1 as !!

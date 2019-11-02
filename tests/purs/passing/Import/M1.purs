module M1 where

id :: forall a. a -> a
id = \x -> x

foo = id

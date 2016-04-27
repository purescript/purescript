module Data.Maybe where

data Maybe a = Just a | Nothing

maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe b _ Nothing = b
maybe _ f (Just a) = f a

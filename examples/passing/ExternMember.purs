module ExternMember where

foreign import member "length" length :: String -> Number

foreign import member "concat" concat :: forall a. [a] -> [a] -> [a]

module Main where

data Proxy c = Proxy

newtype Dict c = Dict (forall r. (c => r) -> r)

dict :: forall c. c => Proxy c -> Dict c
dict _ = Dict \r -> r

-- class RestrictedFunctor (c :: * -> Constraint) f where
--   cfmap :: forall a b. (c a, c b) => (a -> b) -> f a -> f b

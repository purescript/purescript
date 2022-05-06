-- @shouldFailWith NoInstanceFound
module InstanceChains.BothUnknownAndMatch where

import Type.Proxy (Proxy(..))

class Same l r (o :: Symbol) | l r -> o
instance sameY :: Same t t "Y" else instance sameN :: Same l r "N"
same :: forall l r o. Same l r o => l -> r -> Proxy o
same _ _ = Proxy

-- for label `u`, `t ~ Int` should be Unknown
-- for label `m`, `Int ~ Int` should be a match
-- together they should be Unknown
example :: forall t. Proxy t -> Proxy _
example _ = same (Proxy :: Proxy (u :: t, m :: Int))
                 (Proxy :: Proxy (u :: Int, m :: Int))

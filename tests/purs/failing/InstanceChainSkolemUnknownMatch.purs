-- @shouldFailWith NoInstanceFound
module InstanceChainSkolemUnknownMatch where

import Type.Proxy (Proxy(..))

class Same l r (o :: Symbol) | l r -> o
instance sameY :: Same t t "Y" else instance sameN :: Same l r "N"
same :: forall l r o. Same l r o => l -> r -> Proxy o
same _ _ = Proxy

-- shouldn't discard sameY as Apart
example :: forall (t :: Type). Proxy t -> Proxy _
example _ = same (Proxy :: Proxy t) (Proxy :: Proxy Int)


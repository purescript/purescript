-- @shouldFailWith NoInstanceFound
module InstanceChainSkolemUnknownMatch where

import Type.Proxy (Proxy(..))
import Data.Symbol (SProxy(..))

class Same l r (o :: Symbol) | l r -> o
instance sameY :: Same t t "Y" else instance sameN :: Same l r "N"
same :: forall l r o. Same l r o => l -> r -> SProxy o
same _ _ = SProxy

-- shouldn't discard sameY as Apart
example :: forall t. Proxy t -> SProxy _
example _ = same (Proxy :: Proxy t) (Proxy :: Proxy Int)


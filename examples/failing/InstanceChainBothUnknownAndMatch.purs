-- @shouldFailWith NoInstanceFound
module InstanceChains.BothUnknownAndMatch where

import Type.Proxy (Proxy)
import Type.Row (RProxy(..))
import Data.Symbol (SProxy(..))

class Same l r (o :: Symbol) | l r -> o
instance sameY :: Same t t "Y" else instance sameN :: Same l r "N"
same :: forall l r o. Same l r o => l -> r -> SProxy o
same _ _ = SProxy

-- for label `u`, `t ~ Int` should be Unknown
-- for label `m`, `Int ~ Int` should be a match
-- together they should be Unknown
example :: forall t. Proxy t -> SProxy _
example _ = same (RProxy :: RProxy (u :: t, m :: Int))
                 (RProxy :: RProxy (u :: Int, m :: Int))

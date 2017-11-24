-- @shouldFailWith NoInstanceFound
module InstanceChains.BothUnknownAndMatch where

class Same l r o | l r -> o
instance sameY :: Same t t @"Y" else instance sameN :: Same l r @"N"
same :: forall l r o. Same l r o => l -> r -> @o
same _ _ = @o

-- for label `u`, `t ~ Int` should be Unknown
-- for label `m`, `Int ~ Int` should be a match
-- together they should be Unknown
example :: forall t. @t -> @_
example _ = same @(u :: t, m :: Int) @(u :: Int, m :: Int)


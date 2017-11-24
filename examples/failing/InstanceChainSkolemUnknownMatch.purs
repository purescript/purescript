-- @shouldFailWith NoInstanceFound
module InstanceChainSkolemUnknownMatch where

class Same l r o | l r -> o
instance sameY :: Same t t @"Y" else instance sameN :: Same l r @"N"
same :: forall l r o. Same l r o => l -> r -> @o
same _ _ = @o

-- shouldn't discard sameY as Apart
example :: forall t. @t -> @_
example _ = same @t @Int


module NewOperators2 where

infixl 8 _compose as >>>

_compose :: forall a b c. (b -> c) -> (a -> b) -> (a -> c)
_compose f g x = f (g x)

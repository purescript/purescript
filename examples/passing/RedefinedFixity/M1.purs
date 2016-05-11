module M1 where

applyFn :: forall a b. (forall c d. c -> d) -> a -> b
applyFn f a = f a

infixr 1000 applyFn as $

module TypeOpAliases where

type AltFn a b = a -> b

infixr 6 type AltFn as ~>

foreign import test1 :: forall a b. a ~> b
foreign import test2 :: forall a b c. a ~> b ~> c
foreign import test3 :: forall a b c d. a ~> (b ~> c) ~> d
foreign import test4 :: forall a b c d. ((a ~> b) ~> c) ~> d

data Tuple a b = Tuple a b

infixl 6 Tuple as ×
infixl 6 type Tuple as ×

third ∷ ∀ a b c. a × b × c → c
third (a × b × c) = c

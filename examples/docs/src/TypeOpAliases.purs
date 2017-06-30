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

data Boop a b
  = Nope a
  | Welp b

infixr 6 type Boop as </>

class Narp a where
  narp :: a -> String

instance narpBoop :: (Narp a, Narp b) => Narp (a </> b) where
  narp (Nope a) = narp a
  narp (Welp b) = narp b


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

data Either a b = Left a | Right b

infixl 5 type Either as ⊕

third ∷ ∀ a b c. a × b × c → c
third (a × b × c) = c

class Show a where
  show :: a -> String

instance showTuple :: Show a => Show (a × b) where
  show (a × _) = show a

-- Test that precedence is taken into account while desugaring type operators

class TestL a where
  testL :: a

class TestR a where
  testR :: a

-- Note: this type is Either Int (Tuple Int String)
instance testLEither :: TestL (Int ⊕ Int × String) where
  testL = Right (0 × "hi")

-- Note: this type is Either (Tuple Int Int) String
instance testREither :: TestR (Int × Int ⊕ String) where
  testR = Left (0 × 1)

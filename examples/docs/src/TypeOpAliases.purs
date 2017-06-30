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

data BaseType a b
  = Con1 a
  | Con2 b

infixr 6 type BaseType as </>

class TC_A a where
  tC_AFn :: a -> String

instance tC_ABaseType :: (TC_A a, TC_A b) => TC_A (a </> b) where
  tC_AFn (Con1 a) = tC_AFn a
  tC_AFn (Con2 b) = tC_AFn b


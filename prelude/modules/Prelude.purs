module Prelude
  ( otherwise
  , flip
  , const
  , asTypeOf
  , Semigroupoid, (<<<), (>>>)
  , Category, id
  , ($), (#)
  , (:), cons
  , Show, show
  , Functor, (<$>), (<#>), void
  , Apply, (<*>)
  , Applicative, pure, liftA1
  , Bind, (>>=)
  , Monad, return, liftM1, ap
  , Num, (+), (-), (*), (/), (%)
  , negate
  , Eq, (==), (/=), refEq, refIneq
  , Ord, Ordering(..), compare, (<), (>), (<=), (>=)
  , Bits, (.&.), (.|.), (.^.), shl, shr, zshr, complement
  , BoolLike, (&&), (||)
  , not
  , Semigroup, (<>), (++)
  , Unit(..), unit
  ) where

otherwise :: Boolean
otherwise = true

flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f b a = f a b

const :: forall a b. a -> b -> a
const a _ = a

asTypeOf :: forall a. a -> a -> a
asTypeOf x _ = x

infixr 9 >>>
infixr 9 <<<

class Semigroupoid a where
  (<<<) :: forall b c d. a c d -> a b c -> a b d

instance semigroupoidArr :: Semigroupoid (->) where
  (<<<) f g x = f (g x)

(>>>) :: forall a b c d. (Semigroupoid a) => a b c -> a c d -> a b d
(>>>) f g = g <<< f

class (Semigroupoid a) <= Category a where
  id :: forall t. a t t

instance categoryArr :: Category (->) where
  id x = x

infixr 0 $
infixl 0 #

($) :: forall a b. (a -> b) -> a -> b
($) f x = f x

(#) :: forall a b. a -> (a -> b) -> b
(#) x f = f x

infixr 6 :

(:) :: forall a. a -> [a] -> [a]
(:) = cons

foreign import cons
  "function cons(e) {\
  \  return function(l) {\
  \    return [e].concat(l);\
  \  };\
  \}" :: forall a. a -> [a] -> [a]

class Show a where
  show :: a -> String

foreign import showStringImpl
  "function showStringImpl(s) {\
  \  return JSON.stringify(s);\
  \}" :: String -> String

instance showUnit :: Show Unit where
  show (Unit {}) = "Unit {}"

instance showString :: Show String where
  show = showStringImpl

instance showBoolean :: Show Boolean where
  show true = "true"
  show false = "false"

foreign import showNumberImpl "function showNumberImpl(n) {\
                              \  return n.toString();\
                              \}" :: Number -> String

instance showNumber :: Show Number where
  show = showNumberImpl

foreign import showArrayImpl
  "function showArrayImpl(f) {\
  \  return function(xs) {\
  \    var ss = [];\
  \    for (var i = 0, l = xs.length; i < l; i++) {\
  \      ss[i] = f(xs[i]);\
  \    }\
  \    return '[' + ss.join(',') + ']';\
  \  };\
  \}" :: forall a. (a -> String) -> [a] -> String

instance showArray :: (Show a) => Show [a] where
  show = showArrayImpl show

infixl 4 <$>
infixl 1 <#>

class Functor f where
  (<$>) :: forall a b. (a -> b) -> f a -> f b

(<#>) :: forall f a b. (Functor f) => f a -> (a -> b) -> f b
(<#>) fa f = f <$> fa

void :: forall f a. (Functor f) => f a -> f Unit
void fa = const unit <$> fa

infixl 4 <*>

class (Functor f) <= Apply f where
  (<*>) :: forall a b. f (a -> b) -> f a -> f b

class (Apply f) <= Applicative f where
  pure :: forall a. a -> f a

liftA1 :: forall f a b. (Applicative f) => (a -> b) -> f a -> f b
liftA1 f a = pure f <*> a

infixl 1 >>=

class (Apply m) <= Bind m where
  (>>=) :: forall a b. m a -> (a -> m b) -> m b

class (Applicative m, Bind m) <= Monad m

return :: forall m a. (Monad m) => a -> m a
return = pure

liftM1 :: forall m a b. (Monad m) => (a -> b) -> m a -> m b
liftM1 f a = do
  a' <- a
  return (f a')

ap :: forall m a b. (Monad m) => m (a -> b) -> m a -> m b
ap f a = do
  f' <- f
  a' <- a
  return (f' a')

instance functorArr :: Functor ((->) r) where
  (<$>) = (<<<)

instance applyArr :: Apply ((->) r) where
  (<*>) f g x = f x (g x)

instance applicativeArr :: Applicative ((->) r) where
  pure = const

instance bindArr :: Bind ((->) r) where
  (>>=) m f x = f (m x) x

instance monadArr :: Monad ((->) r)

infixl 7 *
infixl 7 /
infixl 7 %

infixl 6 -
infixl 6 +

class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  (/) :: a -> a -> a
  (%) :: a -> a -> a
  negate :: a -> a

foreign import numAdd "function numAdd(n1) {\
                      \  return function(n2) {\
                      \    return n1 + n2;\
                      \  };\
                      \}" :: Number -> Number -> Number

foreign import numSub "function numSub(n1) {\
                      \  return function(n2) {\
                      \    return n1 - n2;\
                      \  };\
                      \}" :: Number -> Number -> Number

foreign import numMul "function numMul(n1) {\
                      \  return function(n2) {\
                      \    return n1 * n2;\
                      \  };\
                      \}" :: Number -> Number -> Number

foreign import numDiv "function numDiv(n1) {\
                      \  return function(n2) {\
                      \    return n1 / n2;\
                      \  };\
                      \}" :: Number -> Number -> Number

foreign import numMod "function numMod(n1) {\
                      \  return function(n2) {\
                      \    return n1 % n2;\
                      \  };\
                      \}" :: Number -> Number -> Number

foreign import numNegate "function numNegate(n) {\
                         \  return -n;\
                         \}" :: Number -> Number

instance numNumber :: Num Number where
  (+) = numAdd
  (-) = numSub
  (*) = numMul
  (/) = numDiv
  (%) = numMod
  negate = numNegate

newtype Unit = Unit {}

unit :: Unit
unit = Unit {}

infix 4 ==
infix 4 /=

class Eq a where
  (==) :: a -> a -> Boolean
  (/=) :: a -> a -> Boolean

foreign import refEq
  "function refEq(r1) {\
  \  return function(r2) {\
  \    return r1 === r2;\
  \  };\
  \}" :: forall a. a -> a -> Boolean

foreign import refIneq
  "function refIneq(r1) {\
  \  return function(r2) {\
  \    return r1 !== r2;\
  \  };\
  \}" :: forall a. a -> a -> Boolean

instance eqUnit :: Eq Unit where
  (==) (Unit {}) (Unit {}) = true
  (/=) (Unit {}) (Unit {}) = false

instance eqString :: Eq String where
  (==) = refEq
  (/=) = refIneq

instance eqNumber :: Eq Number where
  (==) = refEq
  (/=) = refIneq

instance eqBoolean :: Eq Boolean where
  (==) = refEq
  (/=) = refIneq

foreign import eqArrayImpl
  "function eqArrayImpl(f) {\
  \  return function(xs) {\
  \    return function(ys) {\
  \      if (xs.length !== ys.length) return false;\
  \      for (var i = 0; i < xs.length; i++) {\
  \        if (!f(xs[i])(ys[i])) return false;\
  \      }\
  \      return true;\
  \    };\
  \  };\
  \}" :: forall a. (a -> a -> Boolean) -> [a] -> [a] -> Boolean

instance eqArray :: (Eq a) => Eq [a] where
  (==) xs ys = eqArrayImpl (==) xs ys
  (/=) xs ys = not (xs == ys)

data Ordering = LT | GT | EQ

instance eqOrdering :: Eq Ordering where
  (==) LT LT = true
  (==) GT GT = true
  (==) EQ EQ = true
  (==) _  _  = false
  (/=) x y = not (x == y)

instance showOrdering :: Show Ordering where
  show LT = "LT"
  show GT = "GT"
  show EQ = "EQ"

class (Eq a) <= Ord a where
  compare :: a -> a -> Ordering

infixl 4 <

(<) :: forall a. (Ord a) => a -> a -> Boolean
(<) a1 a2 = case a1 `compare` a2 of
  LT -> true
  _ -> false

infixl 4 >

(>) :: forall a. (Ord a) => a -> a -> Boolean
(>) a1 a2 = case a1 `compare` a2 of
  GT -> true
  _ -> false

infixl 4 <=

(<=) :: forall a. (Ord a) => a -> a -> Boolean
(<=) a1 a2 = case a1 `compare` a2 of
  GT -> false
  _ -> true

infixl 4 >=

(>=) :: forall a. (Ord a) => a -> a -> Boolean
(>=) a1 a2 = case a1 `compare` a2 of
  LT -> false
  _ -> true

foreign import unsafeCompareImpl
  "function unsafeCompareImpl(lt) {\
  \  return function(eq) {\
  \    return function(gt) {\
  \      return function(x) {\
  \        return function(y) {\
  \          return x < y ? lt : x > y ? gt : eq;\
  \        };\
  \      };\
  \    };\
  \  };\
  \}" :: forall a. Ordering -> Ordering -> Ordering -> a -> a -> Ordering

unsafeCompare :: forall a. a -> a -> Ordering
unsafeCompare = unsafeCompareImpl LT EQ GT

instance ordUnit :: Ord Unit where
  compare (Unit {}) (Unit {}) = EQ

instance ordBoolean :: Ord Boolean where
  compare false false = EQ
  compare false true  = LT
  compare true  true  = EQ
  compare true  false = GT

instance ordNumber :: Ord Number where
  compare = unsafeCompare

instance ordString :: Ord String where
  compare = unsafeCompare

instance ordArray :: (Ord a) => Ord [a] where
  compare [] [] = EQ
  compare [] _ = LT
  compare _ [] = GT
  compare (x:xs) (y:ys) = case compare x y of
    EQ -> compare xs ys
    other -> other

infixl 10 .&.
infixl 10 .|.
infixl 10 .^.

class Bits b where
  (.&.) :: b -> b -> b
  (.|.) :: b -> b -> b
  (.^.) :: b -> b -> b
  shl :: b -> Number -> b
  shr :: b -> Number -> b
  zshr :: b -> Number -> b
  complement :: b -> b

foreign import numShl "function numShl(n1) {\
                      \  return function(n2) {\
                      \    return n1 << n2;\
                      \  };\
                      \}" :: Number -> Number -> Number

foreign import numShr "function numShr(n1) {\
                      \  return function(n2) {\
                      \    return n1 >> n2;\
                      \  };\
                      \}" :: Number -> Number -> Number

foreign import numZshr "function numZshr(n1) {\
                        \  return function(n2) {\
                        \    return n1 >>> n2;\
                        \  };\
                        \}" :: Number -> Number -> Number

foreign import numAnd "function numAnd(n1) {\
                      \  return function(n2) {\
                      \    return n1 & n2;\
                      \  };\
                      \}" :: Number -> Number -> Number

foreign import numOr "function numOr(n1) {\
                     \  return function(n2) {\
                     \    return n1 | n2;\
                     \  };\
                     \}" :: Number -> Number -> Number

foreign import numXor "function numXor(n1) {\
                      \  return function(n2) {\
                      \    return n1 ^ n2;\
                      \  };\
                      \}" :: Number -> Number -> Number

foreign import numComplement "function numComplement(n) {\
                             \  return ~n;\
                             \}" :: Number -> Number

instance bitsNumber :: Bits Number where
  (.&.) = numAnd
  (.|.) = numOr
  (.^.) = numXor
  shl = numShl
  shr = numShr
  zshr = numZshr
  complement = numComplement

infixr 2 ||
infixr 3 &&

class BoolLike b where
  (&&) :: b -> b -> b
  (||) :: b -> b -> b
  not :: b -> b

foreign import boolAnd "function boolAnd(b1) {\
                       \  return function(b2) {\
                       \    return b1 && b2;\
                       \  };\
                       \}"  :: Boolean -> Boolean -> Boolean

foreign import boolOr "function boolOr(b1) {\
                      \  return function(b2) {\
                      \    return b1 || b2;\
                      \  };\
                      \}" :: Boolean -> Boolean -> Boolean

foreign import boolNot "function boolNot(b) {\
                       \  return !b;\
                       \}" :: Boolean -> Boolean

instance boolLikeBoolean :: BoolLike Boolean where
  (&&) = boolAnd
  (||) = boolOr
  not = boolNot

infixr 5 <>

class Semigroup a where
  (<>) :: a -> a -> a

foreign import concatString
  "function concatString(s1) {\
  \  return function(s2) {\
  \    return s1 + s2;\
  \  };\
  \}" :: String -> String -> String

instance semigroupUnit :: Semigroup Unit where
  (<>) (Unit {}) (Unit {}) = Unit {}

instance semigroupString :: Semigroup String where
  (<>) = concatString

instance semigroupArr :: (Semigroup s') => Semigroup (s -> s') where
  (<>) f g = \x -> f x <> g x

infixr 5 ++

(++) :: forall s. (Semigroup s) => s -> s -> s
(++) = (<>)
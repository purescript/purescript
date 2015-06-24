-- | This snapshot is provided for the purposes of running the test suite.
-- | It is not necessarily kept up to date with the official PureScript Prelude,
-- | and should not be used in user code.
-- | The official Prelude can be found at https://github.com/purescript/purescript-prelude

module Prelude
  ( Unit(..), unit
  , ($), (#)
  , length
  , flip
  , const
  , asTypeOf
  , otherwise
  , (:), cons, concat
  , Semigroupoid, (<<<), (>>>)
  , Category, id
  , Functor, (<$>), (<#>), void
  , Apply, (<*>)
  , Applicative, pure, liftA1
  , Bind, bind, (>>=)
  , Monad, return, liftM1, ap
  , Semigroup, (<>), (++)
  , Semiring, (+), zero, (*), one
  , ModuloSemiring, (/), mod, jsMod, (%)
  , Ring, (-), negate
  , Num
  , DivisionRing
  , Eq, (==), (/=)
  , Ordering(..), Ord, compare, (<), (>), (<=), (>=)
  , Bounded, top, bottom
  , Lattice, sup, inf, (||), (&&)
  , BoundedLattice
  , ComplementedLattice, not
  , DistributiveLattice
  , BooleanAlgebra
  , Show, show
  ) where

  newtype Unit = Unit {}

  unit :: Unit
  unit = Unit {}

  foreign import length :: forall a. Array a -> Number

  infixr 0 $
  infixl 0 #

  ($) :: forall a b. (a -> b) -> a -> b
  ($) f x = f x

  (#) :: forall a b. a -> (a -> b) -> b
  (#) x f = f x

  flip :: forall a b c. (a -> b -> c) -> b -> a -> c
  flip f b a = f a b

  const :: forall a b. a -> b -> a
  const a _ = a

  asTypeOf :: forall a. a -> a -> a
  asTypeOf x _ = x

  otherwise :: Boolean
  otherwise = true

  foreign import cons :: forall a. a -> Array a -> Array a

  infixr 6 :

  (:) :: forall a. a -> Array a -> Array a
  (:) = cons

  foreign import concat :: forall a. Array a -> Array a -> Array a

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

  infixl 4 <$>
  infixl 1 <#>

  class Functor f where
    (<$>) :: forall a b. (a -> b) -> f a -> f b

  instance functorArr :: Functor ((->) r) where
    (<$>) = (<<<)

  (<#>) :: forall f a b. (Functor f) => f a -> (a -> b) -> f b
  (<#>) fa f = f <$> fa

  void :: forall f a. (Functor f) => f a -> f Unit
  void fa = const unit <$> fa

  infixl 4 <*>

  class (Functor f) <= Apply f where
    (<*>) :: forall a b. f (a -> b) -> f a -> f b

  instance applyArr :: Apply ((->) r) where
    (<*>) f g x = f x (g x)

  class (Apply f) <= Applicative f where
    pure :: forall a. a -> f a

  instance applicativeArr :: Applicative ((->) r) where
    pure = const

  return :: forall m a. (Applicative m) => a -> m a
  return = pure

  liftA1 :: forall f a b. (Applicative f) => (a -> b) -> f a -> f b
  liftA1 f a = pure f <*> a

  infixl 1 >>=
 
  (>>=) :: forall m a b. (Bind m) => m a -> (a -> m b) -> m b
  (>>=) = bind

  class (Apply m) <= Bind m where
    bind :: forall a b. m a -> (a -> m b) -> m b

  instance bindArr :: Bind ((->) r) where
    bind m f x = f (m x) x

  class (Applicative m, Bind m) <= Monad m

  instance monadArr :: Monad ((->) r)

  liftM1 :: forall m a b. (Monad m) => (a -> b) -> m a -> m b
  liftM1 f a = do
    a' <- a
    return (f a')

  ap :: forall m a b. (Monad m) => m (a -> b) -> m a -> m b
  ap f a = do
    f' <- f
    a' <- a
    return (f' a')

  infixr 5 <>
  infixr 5 ++

  class Semigroup a where
    (<>) :: a -> a -> a

  (++) :: forall s. (Semigroup s) => s -> s -> s
  (++) = (<>)

  instance semigroupString :: Semigroup String where
    (<>) = concatString

  instance semigroupUnit :: Semigroup Unit where
    (<>) _ _ = unit

  instance semigroupArr :: (Semigroup s') => Semigroup (s -> s') where
    (<>) f g = \x -> f x <> g x

  instance semigroupOrdering :: Semigroup Ordering where
    (<>) LT _ = LT
    (<>) GT _ = GT
    (<>) EQ y = y

  foreign import concatString :: String -> String -> String

  infixl 6 +
  infixl 7 *

  class Semiring a where
    (+)  :: a -> a -> a
    zero :: a
    (*)  :: a -> a -> a
    one  :: a

  instance semiringNumber :: Semiring Number where
    (+) = numAdd
    zero = 0.0
    (*) = numMul
    one = 1.0

  instance semiringUnit :: Semiring Unit where
    (+) _ _ = unit
    zero = unit
    (*) _ _ = unit
    one = unit

  infixl 6 -

  class (Semiring a) <= Ring a where
    (-) :: a -> a -> a

  instance ringNumber :: Ring Number where
    (-) = numSub

  instance ringUnit :: Ring Unit where
    (-) _ _ = unit

  negate :: forall a. (Ring a) => a -> a
  negate a = zero - a

  infixl 7 /

  class (Semiring a) <= ModuloSemiring a where
    (/) :: a -> a -> a
    mod :: a -> a -> a

  instance moduloSemiringNumber :: ModuloSemiring Number where
    (/) = numDiv
    mod _ _ = 0.0

  instance moduloSemiringUnit :: ModuloSemiring Unit where
    (/) _ _ = unit
    mod _ _ = unit

  foreign import jsMod :: Number -> Number -> Number

  infixl 7 %

  (%) = jsMod

  class (Ring a, ModuloSemiring a) <= DivisionRing a

  instance divisionRingNumber :: DivisionRing Number

  instance divisionRingUnit :: DivisionRing Unit

  class (DivisionRing a) <= Num a

  instance numNumber :: Num Number

  instance numUnit :: Num Unit

  foreign import numAdd :: Number -> Number -> Number

  foreign import numMul :: Number -> Number -> Number

  foreign import numDiv :: Number -> Number -> Number

  foreign import numSub :: Number -> Number -> Number

  infix 4 ==
  infix 4 /=

  class Eq a where
    (==) :: a -> a -> Boolean
    (/=) :: a -> a -> Boolean

  instance eqBoolean :: Eq Boolean where
    (==) = refEq
    (/=) = refIneq

  instance eqNumber :: Eq Number where
    (==) = refEq
    (/=) = refIneq

  instance eqInt :: Eq Int where
    (==) = refEq
    (/=) = refIneq

  instance eqChar :: Eq Char where
    (==) = refEq
    (/=) = refIneq

  instance eqString :: Eq String where
    (==) = refEq
    (/=) = refIneq

  instance eqUnit :: Eq Unit where
    (==) _ _ = true
    (/=) _ _ = false

  instance eqArray :: (Eq a) => Eq (Array a) where
    (==) = eqArrayImpl (==)
    (/=) xs ys = not (xs == ys)

  instance eqOrdering :: Eq Ordering where
    (==) LT LT = true
    (==) GT GT = true
    (==) EQ EQ = true
    (==) _  _  = false
    (/=) x y = not (x == y)

  foreign import refEq :: forall a. a -> a -> Boolean

  foreign import refIneq :: forall a. a -> a -> Boolean

  foreign import eqArrayImpl :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Boolean

  data Ordering = LT | GT | EQ

  class (Eq a) <= Ord a where
    compare :: a -> a -> Ordering

  instance ordBoolean :: Ord Boolean where
    compare false false = EQ
    compare false true  = LT
    compare true  true  = EQ
    compare true  false = GT

  instance ordNumber :: Ord Number where
    compare = unsafeCompare

  instance ordString :: Ord String where
    compare = unsafeCompare

  instance ordUnit :: Ord Unit where
    compare _ _ = EQ

  instance ordArray :: (Ord a) => Ord (Array a) where
    compare xs ys = compare 0.0 $ ordArrayImpl (\x y -> case compare x y of
                                                EQ -> 0.0
                                                LT -> -1.0
                                                GT -> 1.0) xs ys

  foreign import ordArrayImpl :: forall a. (a -> a -> Number) -> Array a -> Array a -> Number

  instance ordOrdering :: Ord Ordering where
    compare LT LT = EQ
    compare EQ EQ = EQ
    compare GT GT = EQ
    compare LT _  = LT
    compare EQ LT = GT
    compare EQ GT = LT
    compare GT _  = GT

  infixl 4 <
  infixl 4 >
  infixl 4 <=
  infixl 4 >=

  (<) :: forall a. (Ord a) => a -> a -> Boolean
  (<) a1 a2 = case a1 `compare` a2 of
    LT -> true
    _ -> false

  (>) :: forall a. (Ord a) => a -> a -> Boolean
  (>) a1 a2 = case a1 `compare` a2 of
    GT -> true
    _ -> false

  (<=) :: forall a. (Ord a) => a -> a -> Boolean
  (<=) a1 a2 = case a1 `compare` a2 of
    GT -> false
    _ -> true

  (>=) :: forall a. (Ord a) => a -> a -> Boolean
  (>=) a1 a2 = case a1 `compare` a2 of
    LT -> false
    _ -> true

  unsafeCompare :: forall a. a -> a -> Ordering
  unsafeCompare = unsafeCompareImpl LT EQ GT

  foreign import unsafeCompareImpl :: forall a. Ordering -> Ordering -> Ordering -> a -> a -> Ordering

  class (Ord a) <= Bounded a where
    top :: a
    bottom :: a

  instance boundedBoolean :: Bounded Boolean where
    top = true
    bottom = false

  instance boundedUnit :: Bounded Unit where
    top = unit
    bottom = unit

  instance boundedOrdering :: Bounded Ordering where
    top = GT
    bottom = LT

  class (Ord a) <= Lattice a where
    sup :: a -> a -> a
    inf :: a -> a -> a

  instance latticeBoolean :: Lattice Boolean where
    sup = boolOr
    inf = boolAnd

  instance latticeUnit :: Lattice Unit where
    sup _ _ = unit
    inf _ _ = unit

  infixr 2 ||
  infixr 3 &&

  (||) :: forall a. (Lattice a) => a -> a -> a
  (||) = sup

  (&&) :: forall a. (Lattice a) => a -> a -> a
  (&&) = inf

  class (Bounded a, Lattice a) <= BoundedLattice a

  instance boundedLatticeBoolean :: BoundedLattice Boolean

  instance boundedLatticeUnit :: BoundedLattice Unit

  class (BoundedLattice a) <= ComplementedLattice a where
    not :: a -> a

  instance complementedLatticeBoolean :: ComplementedLattice Boolean where
    not = boolNot

  instance complementedLatticeUnit :: ComplementedLattice Unit where
    not _ = unit

  class (Lattice a) <= DistributiveLattice a

  instance distributiveLatticeBoolean :: DistributiveLattice Boolean

  instance distributiveLatticeUnit :: DistributiveLattice Unit

  class (ComplementedLattice a, DistributiveLattice a) <= BooleanAlgebra a

  instance booleanAlgebraBoolean :: BooleanAlgebra Boolean

  instance booleanAlgebraUnit :: BooleanAlgebra Unit

  foreign import boolOr :: Boolean -> Boolean -> Boolean

  foreign import boolAnd :: Boolean -> Boolean -> Boolean

  foreign import boolNot :: Boolean -> Boolean

  class Show a where
    show :: a -> String

  instance showBoolean :: Show Boolean where
    show true = "true"
    show false = "false"

  instance showNumber :: Show Number where
    show = showNumberImpl

  instance showString :: Show String where
    show = showStringImpl

  instance showUnit :: Show Unit where
    show _ = "unit"

  instance showArray :: (Show a) => Show (Array a) where
    show = showArrayImpl show

  instance showOrdering :: Show Ordering where
    show LT = "LT"
    show GT = "GT"
    show EQ = "EQ"

  foreign import showNumberImpl :: Number -> String

  foreign import showStringImpl :: String -> String

  foreign import showArrayImpl :: forall a. (a -> String) -> Array a -> String

module Data.Function where

  import Prelude

  on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
  on f g x y = g x `f` g y

  foreign import data Fn0 :: * -> *

  foreign import data Fn1 :: * -> * -> *

  foreign import data Fn2 :: * -> * -> * -> *

  foreign import data Fn3 :: * -> * -> * -> * -> *

  foreign import data Fn4 :: * -> * -> * -> * -> * -> *

  foreign import data Fn5 :: * -> * -> * -> * -> * -> * -> *

  foreign import data Fn6 :: * -> * -> * -> * -> * -> * -> * -> *

  foreign import data Fn7 :: * -> * -> * -> * -> * -> * -> * -> * -> *

  foreign import data Fn8 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> *

  foreign import data Fn9 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *

  foreign import data Fn10 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *

  foreign import mkFn0 :: forall a. (Unit -> a) -> Fn0 a

  foreign import mkFn1 :: forall a b. (a -> b) -> Fn1 a b

  foreign import mkFn2 :: forall a b c. (a -> b -> c) -> Fn2 a b c

  foreign import mkFn3 :: forall a b c d. (a -> b -> c -> d) -> Fn3 a b c d

  foreign import mkFn4 :: forall a b c d e. (a -> b -> c -> d -> e) -> Fn4 a b c d e

  foreign import mkFn5 :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Fn5 a b c d e f

  foreign import mkFn6 :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Fn6 a b c d e f g

  foreign import mkFn7 :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> Fn7 a b c d e f g h

  foreign import mkFn8 :: forall a b c d e f g h i. (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Fn8 a b c d e f g h i

  foreign import mkFn9 :: forall a b c d e f g h i j. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Fn9 a b c d e f g h i j

  foreign import mkFn10 :: forall a b c d e f g h i j k. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Fn10 a b c d e f g h i j k

  foreign import runFn0 :: forall a. Fn0 a -> a

  foreign import runFn1 :: forall a b. Fn1 a b -> a -> b

  foreign import runFn2 :: forall a b c. Fn2 a b c -> a -> b -> c

  foreign import runFn3 :: forall a b c d. Fn3 a b c d -> a -> b -> c -> d

  foreign import runFn4 :: forall a b c d e. Fn4 a b c d e -> a -> b -> c -> d -> e

  foreign import runFn5 :: forall a b c d e f. Fn5 a b c d e f -> a -> b -> c -> d -> e -> f

  foreign import runFn6 :: forall a b c d e f g. Fn6 a b c d e f g -> a -> b -> c -> d -> e -> f -> g

  foreign import runFn7 :: forall a b c d e f g h. Fn7 a b c d e f g h -> a -> b -> c -> d -> e -> f -> g -> h

  foreign import runFn8 :: forall a b c d e f g h i. Fn8 a b c d e f g h i -> a -> b -> c -> d -> e -> f -> g -> h -> i

  foreign import runFn9 :: forall a b c d e f g h i j. Fn9 a b c d e f g h i j -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j

  foreign import runFn10 :: forall a b c d e f g h i j k. Fn10 a b c d e f g h i j k -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k

module Assert where

  import Prelude
  import Control.Monad.Eff

  foreign import data Assert :: !

  foreign import error :: forall a. String -> a

  foreign import assertPartial :: forall e a. (Unit -> a) -> Eff (assert :: Assert | e) Unit

  assert :: forall e. Boolean -> Eff (assert :: Assert | e) Unit
  assert true = return unit
  assert false = error "Assertion failed!"

module Prelude.Unsafe where

  foreign import unsafeIndex :: forall a. Array a -> Number -> a

module Control.Monad.Eff
  ( Eff()
  , Pure()
  , runPure
  , untilE, whileE
  ) where

  import Prelude

  foreign import data Eff :: # ! -> * -> *

  foreign import returnE :: forall e a. a -> Eff e a

  foreign import bindE :: forall e a b. Eff e a -> (a -> Eff e b) -> Eff e b

  type Pure a = forall e. Eff e a

  foreign import runPure :: forall a. Pure a -> a

  instance functorEff :: Functor (Eff e) where
    (<$>) = liftA1

  instance applyEff :: Apply (Eff e) where
    (<*>) = ap

  instance applicativeEff :: Applicative (Eff e) where
    pure = returnE

  instance bindEff :: Bind (Eff e) where
    bind = bindE

  instance monadEff :: Monad (Eff e)

  foreign import untilE :: forall e. Eff e Boolean -> Eff e Unit

  foreign import whileE :: forall e a. Eff e Boolean -> Eff e a -> Eff e Unit

module Debug.Trace where

  import Prelude
  import Control.Monad.Eff

  foreign import data Trace :: !

  foreign import trace :: forall r. String -> Eff (trace :: Trace | r) Unit

  print :: forall a r. (Show a) => a -> Eff (trace :: Trace | r) Unit
  print o = trace (show o)

module Control.Monad.ST where

  import Prelude
  import Control.Monad.Eff

  foreign import data ST :: * -> !

  foreign import data STRef :: * -> * -> *

  foreign import newSTRef :: forall a h r. a -> Eff (st :: ST h | r) (STRef h a)

  foreign import readSTRef :: forall a h r. STRef h a -> Eff (st :: ST h | r) a

  foreign import modifySTRef :: forall a h r. STRef h a -> (a -> a) -> Eff (st :: ST h | r) a

  foreign import writeSTRef :: forall a h r. STRef h a -> a -> Eff (st :: ST h | r) a

  foreign import runST :: forall a r. (forall h. Eff (st :: ST h | r) a) -> Eff r a

  pureST :: forall a. (forall h r. Eff (st :: ST h | r) a) -> a
  pureST st = runPure (runST st)

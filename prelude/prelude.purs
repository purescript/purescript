module Prelude
  ( Unit(..), unit
  , ($), (#)
  , flip
  , const
  , asTypeOf
  , otherwise
  , (:), cons
  , Semigroupoid, (<<<), (>>>)
  , Category, id
  , Functor, (<$>), (<#>), void
  , Apply, (<*>)
  , Applicative, pure, liftA1
  , Bind, (>>=)
  , Monad, return, liftM1, ap
  , Semigroup, (<>), (++)
  , Semiring, (+), zero, (*), one
  , ModuloSemiring, (/), mod
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

  -- | The `Unit` type has a single inhabitant, called `unit`. It represents
  -- | values with no computational content.
  -- |
  -- | `Unit` is often used, wrapped in a monadic type constructor, as the
  -- | return type of a computation where only
  -- | the _effects_ are important.
  newtype Unit = Unit {}

  -- | `unit` is the sole inhabitant of the `Unit` type.
  unit :: Unit
  unit = Unit {}

  infixr 0 $
  infixl 0 #

  -- | Applies a function to its argument.
  -- |
  -- | ```purescript
  -- | length $ groupBy productCategory $ filter isInStock $ products
  -- | ```
  -- |
  -- | is equivalent to:
  -- |
  -- | ```purescript
  -- | length (groupBy productCategory (filter isInStock products))
  -- | ```
  -- |
  -- | `($)` is different from [`(#)`](#-2) because it is right-infix instead of
  -- | left: `a $ b $ c $ d x = a $ (b $ (c $ (d $ x))) = a (b (c (d x)))`
  ($) :: forall a b. (a -> b) -> a -> b
  ($) f x = f x

  -- | Applies an argument to a function.
  -- |
  -- | ```purescript
  -- | products # filter isInStock # groupBy productCategory # length
  -- | ```
  -- |
  -- | is equivalent to:
  -- |
  -- | ```purescript
  -- | length (groupBy productCategory (filter isInStock products))
  -- | ```
  -- |
  -- | `(#)` is different from [`($)`](#-1) because it is left-infix instead of
  -- | right: `x # a # b # c # d = (((x # a) # b) # c) # d = d (c (b (a x)))`
  (#) :: forall a b. a -> (a -> b) -> b
  (#) x f = f x

  -- | Flips the order of the arguments to a function of two arguments.
  -- |
  -- | ```purescript
  -- | flip const 1 2 = const 2 1 = 2
  -- | ```
  flip :: forall a b c. (a -> b -> c) -> b -> a -> c
  flip f b a = f a b

  -- | Returns its first argument and ignores its second.
  -- |
  -- | ```purescript
  -- | const 1 "hello" = 1
  -- | ```
  const :: forall a b. a -> b -> a
  const a _ = a

  -- | This function returns its first argument, and can be used to assert type
  -- | equalities. This can be useful when types are otherwise ambiguous.
  -- |
  -- | ```purescript
  -- | main = print $ [] `asTypeOf` [0]
  -- | ```
  -- |
  -- | If instead, we had written `main = print []`, the type of the argument
  -- | `[]` would have been ambiguous, resulting in a compile-time error.
  asTypeOf :: forall a. a -> a -> a
  asTypeOf x _ = x

  -- | An alias for `true`, which can be useful in guard clauses:
  -- |
  -- | ```purescript
  -- | max x y | x >= y    = x
  -- |         | otherwise = y
  -- | ```
  otherwise :: Boolean
  otherwise = true

  -- | Attaches an element to the front of an array, creating a new array.
  -- |
  -- | ```purescript
  -- | cons 1 [2, 3, 4] = [1, 2, 3, 4]
  -- | ```
  -- |
  -- | Note, the running time of this function is `O(n)`.
  foreign import cons
    """
    function cons(e) {
      return function(l) {
        return [e].concat(l);
      };
    }
    """ :: forall a. a -> [a] -> [a]

  infixr 6 :

  -- | An infix alias for `cons`.
  -- |
  -- | Note, the running time of this function is `O(n)`.
  (:) :: forall a. a -> [a] -> [a]
  (:) = cons

  infixr 9 >>>
  infixr 9 <<<

  -- | A `Semigroupoid` is similar to a [`Category`](#category) but does not
  -- | require an identity element `id`, just composable morphisms.
  -- |
  -- | `Semigroupoid`s must satisfy the following law:
  -- |
  -- | - Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`
  -- |
  -- | One example of a `Semigroupoid` is the function type constructor `(->)`,
  -- | with `(<<<)` defined as function composition.
  class Semigroupoid a where
    (<<<) :: forall b c d. a c d -> a b c -> a b d

  instance semigroupoidArr :: Semigroupoid (->) where
    (<<<) f g x = f (g x)

  -- | Forwards composition, or `(<<<)` with its arguments reversed.
  (>>>) :: forall a b c d. (Semigroupoid a) => a b c -> a c d -> a b d
  (>>>) f g = g <<< f

  -- | `Category`s consist of objects and composable morphisms between them, and
  -- | as such are [`Semigroupoids`](#semigroupoid), but unlike `semigroupoids`
  -- | must have an identity element.
  -- |
  -- | Instances must satisfy the following law in addition to the
  -- | `Semigroupoid` law:
  -- |
  -- | - Identity: `id <<< p = p <<< id = p`
  class (Semigroupoid a) <= Category a where
    id :: forall t. a t t

  instance categoryArr :: Category (->) where
    id x = x

  infixl 4 <$>
  infixl 1 <#>

  -- | A `Functor` is a type constructor which supports a mapping operation
  -- | `(<$>)`.
  -- |
  -- | `(<$>)` can be used to turn functions `a -> b` into functions
  -- | `f a -> f b` whose argument and return types use the type constructor `f`
  -- | to represent some computational context.
  -- |
  -- | Instances must satisfy the following laws:
  -- |
  -- | - Identity: `(<$>) id = id`
  -- | - Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`
  class Functor f where
    (<$>) :: forall a b. (a -> b) -> f a -> f b

  instance functorArr :: Functor ((->) r) where
    (<$>) = (<<<)

  -- | `(<#>)` is `(<$>)` with its arguments reversed. For example:
  -- |
  -- | ```purescript
  -- | [1, 2, 3] <#> \n -> n * n
  -- | ```
  (<#>) :: forall f a b. (Functor f) => f a -> (a -> b) -> f b
  (<#>) fa f = f <$> fa

  -- | The `void` function is used to ignore the type wrapped by a
  -- | [`Functor`](#functor), replacing it with `Unit` and keeping only the type
  -- | information provided by the type constructor itself.
  -- |
  -- | `void` is often useful when using `do` notation to change the return type
  -- | of a monadic computation:
  -- |
  -- | ```purescript
  -- | main = forE 1 10 \n -> void do
  -- |   print n
  -- |   print (n * n)
  -- | ```
  void :: forall f a. (Functor f) => f a -> f Unit
  void fa = const unit <$> fa

  infixl 4 <*>

  -- | The `Apply` class provides the `(<*>)` which is used to apply a function
  -- | to an argument under a type constructor.
  -- |
  -- | `Apply` can be used to lift functions of two or more arguments to work on
  -- | values wrapped with the type constructor `f`. It might also be understood
  -- | in terms of the `lift2` function:
  -- |
  -- | ```purescript
  -- | lift2 :: forall f a b c. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
  -- | lift2 f a b = f <$> a <*> b
  -- | ```
  -- |
  -- | `(<*>)` is recovered from `lift2` as `lift2 ($)`. That is, `(<*>)` lifts
  -- | the function application operator `($)` to arguments wrapped with the
  -- | type constructor `f`.
  -- |
  -- | Instances must satisfy the following law in addition to the `Functor`
  -- | laws:
  -- |
  -- | - Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`
  -- |
  -- | Formally, `Apply` represents a strong lax semi-monoidal endofunctor.
  class (Functor f) <= Apply f where
    (<*>) :: forall a b. f (a -> b) -> f a -> f b

  instance applyArr :: Apply ((->) r) where
    (<*>) f g x = f x (g x)

  -- | The `Applicative` type class extends the [`Apply`](#apply) type class
  -- | with a `pure` function, which can be used to create values of type `f a`
  -- | from values of type `a`.
  -- |
  -- | Where [`Apply`](#apply) provides the ability to lift functions of two or
  -- | more arguments to functions whose arguments are wrapped using `f`, and
  -- | [`Functor`](#functor) provides the ability to lift functions of one
  -- | argument, `pure` can be seen as the function which lifts functions of
  -- | _zero_ arguments. That is, `Applicative` functors support a lifting
  -- | operation for any number of function arguments.
  -- |
  -- | Instances must satisfy the following laws in addition to the `Apply`
  -- | laws:
  -- |
  -- | - Identity: `(pure id) <*> v = v`
  -- | - Composition: `(pure <<<) <*> f <*> g <*> h = f <*> (g <*> h)`
  -- | - Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
  -- | - Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`
  class (Apply f) <= Applicative f where
    pure :: forall a. a -> f a

  instance applicativeArr :: Applicative ((->) r) where
    pure = const

  -- | `return` is an alias for `pure`.
  return :: forall m a. (Applicative m) => a -> m a
  return = pure

  -- | `liftA1` provides a default implementation of `(<$>)` for any
  -- | [`Applicative`](#applicative) functor, without using `(<$>)` as provided
  -- | by the [`Functor`](#functor)-[`Applicative`](#applicative) superclass
  -- | relationship.
  -- |
  -- | `liftA1` can therefore be used to write [`Functor`](#functor) instances
  -- | as follows:
  -- |
  -- | ```purescript
  -- | instance functorF :: Functor F where
  -- |   (<$>) = liftA1
  -- | ```
  liftA1 :: forall f a b. (Applicative f) => (a -> b) -> f a -> f b
  liftA1 f a = pure f <*> a

  infixl 1 >>=

  -- | The `Bind` type class extends the [`Apply`](#apply) type class with a
  -- | "bind" operation `(>>=)` which composes computations in sequence, using
  -- | the return value of one computation to determine the next computation.
  -- |
  -- | The `>>=` operator can also be expressed using `do` notation, as follows:
  -- |
  -- | ```purescript
  -- | x >>= f = do y <- x
  -- |              f y
  -- | ```
  -- |
  -- | where the function argument of `f` is given the name `y`.
  -- |
  -- | Instances must satisfy the following law in addition to the `Apply`
  -- | laws:
  -- |
  -- | - Associativity: `(x >>= f) >>= g = x >>= (\k => f k >>= g)`
  -- |
  -- | Associativity tells us that we can regroup operations which use `do`
  -- | notation so that we can unambiguously write, for example:
  -- |
  -- | ```purescript
  -- | do x <- m1
  -- |    y <- m2 x
  -- |    m3 x y
  -- | ```
  class (Apply m) <= Bind m where
    (>>=) :: forall a b. m a -> (a -> m b) -> m b

  instance bindArr :: Bind ((->) r) where
    (>>=) m f x = f (m x) x

  -- | The `Monad` type class combines the operations of the `Bind` and
  -- | `Applicative` type classes. Therefore, `Monad` instances represent type
  -- | constructors which support sequential composition, and also lifting of
  -- | functions of arbitrary arity.
  -- |
  -- | Instances must satisfy the following laws in addition to the
  -- | `Applicative` and `Bind` laws:
  -- |
  -- | - Left Identity: `pure x >>= f = f x`
  -- | - Right Identity: `x >>= pure = x`
  class (Applicative m, Bind m) <= Monad m

  instance monadArr :: Monad ((->) r)

  -- | `liftM1` provides a default implementation of `(<$>)` for any
  -- | [`Monad`](#monad), without using `(<$>)` as provided by the
  -- | [`Functor`](#functor)-[`Monad`](#monad) superclass relationship.
  -- |
  -- | `liftM1` can therefore be used to write [`Functor`](#functor) instances
  -- | as follows:
  -- |
  -- | ```purescript
  -- | instance functorF :: Functor F where
  -- |   (<$>) = liftM1
  -- | ```
  liftM1 :: forall m a b. (Monad m) => (a -> b) -> m a -> m b
  liftM1 f a = do
    a' <- a
    return (f a')

  -- | `ap` provides a default implementation of `(<*>)` for any
  -- | [`Monad`](#monad), without using `(<*>)` as provided by the
  -- | [`Apply`](#apply)-[`Monad`](#monad) superclass relationship.
  -- |
  -- | `ap` can therefore be used to write [`Apply`](#apply) instances as
  -- | follows:
  -- |
  -- | ```purescript
  -- | instance applyF :: Apply F where
  -- |   (<*>) = ap
  -- | ```
  ap :: forall m a b. (Monad m) => m (a -> b) -> m a -> m b
  ap f a = do
    f' <- f
    a' <- a
    return (f' a')

  infixr 5 <>
  infixr 5 ++

  -- | The `Semigroup` type class identifies an associative operation on a type.
  -- |
  -- | Instances are required to satisfy the following law:
  -- |
  -- | - Associativity: `(x <> y) <> z = x <> (y <> z)`
  -- |
  -- | One example of a `Semigroup` is `String`, with `(<>)` defined as string
  -- | concatenation.
  class Semigroup a where
    (<>) :: a -> a -> a

  -- | `(++)` is an alias for `(<>)`.
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

  foreign import concatString
    """
    function concatString(s1) {
      return function(s2) {
        return s1 + s2;
      };
    }
    """ :: String -> String -> String

  infixl 6 +
  infixl 7 *

  -- | The `Semiring` class is for types that support an addition and
  -- | multiplication operation.
  -- |
  -- | Instances must satisfy the following laws:
  -- |
  -- | - Commutative monoid under addition:
  -- |   - Associativity: `(a + b) + c = a + (b + c)`
  -- |   - Identity: `zero + a = a + zero = a`
  -- |   - Commutative: `a + b = b + a`
  -- | - Monoid under multiplication:
  -- |   - Associativity: `(a * b) * c = a * (b * c)`
  -- |   - Identity: `one * a = a * one = a`
  -- | - Multiplication distributes over addition:
  -- |   - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
  -- |   - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
  -- | - Annihiliation: `zero * a = a * zero = zero`
  class Semiring a where
    (+)  :: a -> a -> a
    zero :: a
    (*)  :: a -> a -> a
    one  :: a

  instance semiringNumber :: Semiring Number where
    (+) = numAdd
    zero = 0
    (*) = numMul
    one = 1

  instance semiringUnit :: Semiring Unit where
    (+) _ _ = unit
    zero = unit
    (*) _ _ = unit
    one = unit

  infixl 6 -

  -- | The `Ring` class is for types that support addition, multiplication,
  -- | and subtraction operations.
  -- |
  -- | Instances must satisfy the following law in addition to the `Semiring`
  -- | laws:
  -- |
  -- | - Additive inverse: `a + (-a) = (-a) + a = zero`
  class (Semiring a) <= Ring a where
    (-) :: a -> a -> a

  instance ringNumber :: Ring Number where
    (-) = numSub

  instance ringUnit :: Ring Unit where
    (-) _ _ = unit

  negate :: forall a. (Ring a) => a -> a
  negate a = zero - a

  infixl 7 /

  -- | The `ModuloSemiring` class is for types that support addition,
  -- | multiplication, division, and modulo (division remainder) operations.
  -- |
  -- | Instances must satisfy the following law in addition to the `Semiring`
  -- | laws:
  -- |
  -- | - Remainder: `a / b * b + (a `mod` b) = a`
  class (Semiring a) <= ModuloSemiring a where
    (/) :: a -> a -> a
    mod :: a -> a -> a

  instance moduloSemiringNumber :: ModuloSemiring Number where
    (/) = numDiv
    mod _ _ = 0

  instance moduloSemiringUnit :: ModuloSemiring Unit where
    (/) _ _ = unit
    mod _ _ = unit

  -- | A `Ring` where every nonzero element has a multiplicative inverse.
  -- |
  -- | Instances must satisfy the following law in addition to the `Ring` and
  -- | `ModuloSemiring` laws:
  -- |
  -- | - Multiplicative inverse: `(one / x) * x = one`
  -- |
  -- | As a consequence of this ```a `mod` b = zero``` as no divide operation
  -- | will have a remainder.
  class (Ring a, ModuloSemiring a) <= DivisionRing a

  instance divisionRingNumber :: DivisionRing Number

  instance divisionRingUnit :: DivisionRing Unit

  -- | The `Num` class is for types that are commutative fields.
  -- |
  -- | Instances must satisfy the following law in addition to the
  -- | `DivisionRing` laws:
  -- |
  -- | - Commutative multiplication: `a * b = b * a`
  class (DivisionRing a) <= Num a

  instance numNumber :: Num Number

  instance numUnit :: Num Unit

  foreign import numAdd
    """
    function numAdd(n1) {
      return function(n2) {
        return n1 + n2;
      };
    }
    """ :: Number -> Number -> Number

  foreign import numMul
    """
    function numMul(n1) {
      return function(n2) {
        return n1 * n2;
      };
    }
    """ :: Number -> Number -> Number

  foreign import numDiv
    """
    function numDiv(n1) {
      return function(n2) {
        return n1 / n2;
      };
    }
    """ :: Number -> Number -> Number

  foreign import numSub
    """
    function numSub(n1) {
      return function(n2) {
        return n1 - n2;
      };
    }
    """ :: Number -> Number -> Number

  infix 4 ==
  infix 4 /=

  -- | The `Eq` type class represents types which support decidable equality.
  -- |
  -- | `Eq` instances should satisfy the following laws:
  -- |
  -- | - Reflexivity: `x == x = true`
  -- | - Symmetry: `x == y = y == x`
  -- | - Transitivity: if `x == y` and `y == z` then `x == z`
  -- | - Negation: `x /= y = not (x == y)`
  -- |
  -- | `(/=)` may be implemented in terms of `(==)`, but it might give a performance improvement to implement it separately.
  class Eq a where
    (==) :: a -> a -> Boolean
    (/=) :: a -> a -> Boolean

  instance eqBoolean :: Eq Boolean where
    (==) = refEq
    (/=) = refIneq

  instance eqNumber :: Eq Number where
    (==) = refEq
    (/=) = refIneq

  instance eqString :: Eq String where
    (==) = refEq
    (/=) = refIneq

  instance eqUnit :: Eq Unit where
    (==) _ _ = true
    (/=) _ _ = false

  instance eqArray :: (Eq a) => Eq [a] where
    (==) = eqArrayImpl (==)
    (/=) xs ys = not (xs == ys)

  instance eqOrdering :: Eq Ordering where
    (==) LT LT = true
    (==) GT GT = true
    (==) EQ EQ = true
    (==) _  _  = false
    (/=) x y = not (x == y)

  foreign import refEq
    """
    function refEq(r1) {
      return function(r2) {
        return r1 === r2;
      };
    }
    """ :: forall a. a -> a -> Boolean

  foreign import refIneq
    """
    function refIneq(r1) {
      return function(r2) {
        return r1 !== r2;
      };
    }
    """ :: forall a. a -> a -> Boolean

  foreign import eqArrayImpl
    """
    function eqArrayImpl(f) {
      return function(xs) {
        return function(ys) {
          if (xs.length !== ys.length) return false;
          for (var i = 0; i < xs.length; i++) {
            if (!f(xs[i])(ys[i])) return false;
          }
          return true;
        };
      };
    }
    """ :: forall a. (a -> a -> Boolean) -> [a] -> [a] -> Boolean

  -- | The `Ordering` data type represents the three possible outcomes of
  -- | comparing two values:
  -- |
  -- | `LT` - The first value is _less than_ the second.
  -- | `GT` - The first value is _greater than_ the second.
  -- | `EQ` - The first value is _equal to_ or _incomparable to_ the second.
  data Ordering = LT | GT | EQ

  -- | The `Ord` type class represents types which support comparisons.
  -- |
  -- | `Ord` instances should satisfy the laws of _partially orderings_:
  -- |
  -- | - Reflexivity: `a <= a`
  -- | - Antisymmetry: if `a <= b` and `b <= a` then `a = b`
  -- | - Transitivity: if `a <= b` and `b <= c` then `a <= c`
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

  instance ordArray :: (Ord a) => Ord [a] where
    compare [] [] = EQ
    compare [] _ = LT
    compare _ [] = GT
    compare (x:xs) (y:ys) = case compare x y of
      EQ -> compare xs ys
      other -> other

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

  -- | Test whether one value is _strictly less than_ another.
  (<) :: forall a. (Ord a) => a -> a -> Boolean
  (<) a1 a2 = case a1 `compare` a2 of
    LT -> true
    _ -> false

  -- | Test whether one value is _strictly greater than_ another.
  (>) :: forall a. (Ord a) => a -> a -> Boolean
  (>) a1 a2 = case a1 `compare` a2 of
    GT -> true
    _ -> false

  -- | Test whether one value is _non-strictly less than_ another.
  (<=) :: forall a. (Ord a) => a -> a -> Boolean
  (<=) a1 a2 = case a1 `compare` a2 of
    GT -> false
    _ -> true

  -- | Test whether one value is _non-strictly greater than_ another.
  (>=) :: forall a. (Ord a) => a -> a -> Boolean
  (>=) a1 a2 = case a1 `compare` a2 of
    LT -> false
    _ -> true

  unsafeCompare :: forall a. a -> a -> Ordering
  unsafeCompare = unsafeCompareImpl LT EQ GT

  foreign import unsafeCompareImpl
    """
    function unsafeCompareImpl(lt) {
      return function(eq) {
        return function(gt) {
          return function(x) {
            return function(y) {
              return x < y ? lt : x > y ? gt : eq;
            };
          };
        };
      };
    }
    """ :: forall a. Ordering -> Ordering -> Ordering -> a -> a -> Ordering

  -- | The `Bounded` type class represents types that are finite partially
  -- | ordered sets.
  -- |
  -- | Instances should satisfy the following law in addition to the `Ord` laws:
  -- |
  -- | - Ordering: `bottom <= a <= top`
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

  -- | The `Lattice` type class represents types that are partially ordered
  -- | sets with a supremum (`sup` or `||`) and infimum (`inf` or `&&`).
  -- |
  -- | Instances should satisfy the following laws in addition to the `Ord`
  -- | laws:
  -- |
  -- | - Associativity:
  -- |   - `a || (b || c) = (a || b) || c`
  -- |   - `a && (b && c) = (a && b) && c`
  -- | - Commutativity:
  -- |   - `a || b = b || a`
  -- |   - `a && b = b && a`
  -- | - Absorption:
  -- |   - `a || (a && b) = a`
  -- |   - `a && (a || b) = a`
  -- | - Idempotent:
  -- |   - `a || a = a`
  -- |   - `a && a = a`
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

  -- | The `sup` operator.
  (||) :: forall a. (Lattice a) => a -> a -> a
  (||) = sup

  -- | The `inf` operator.
  (&&) :: forall a. (Lattice a) => a -> a -> a
  (&&) = inf

  -- | The `BoundedLattice` type class represents types that are finite
  -- | lattices.
  -- |
  -- | Instances should satisfy the following law in addition to the `Lattice`
  -- | and `Bounded` laws:
  -- |
  -- | - Identity:
  -- |   - `a || bottom = a`
  -- |   - `a && top = a`
  -- | - Annihiliation:
  -- |   - `a || top = top`
  -- |   - `a && bottom = bottom`
  class (Bounded a, Lattice a) <= BoundedLattice a

  instance boundedLatticeBoolean :: BoundedLattice Boolean

  instance boundedLatticeUnit :: BoundedLattice Unit

  -- | The `ComplementedLattice` type class represents types that are lattices
  -- | where every member is also uniquely complemented.
  -- |
  -- | Instances should satisfy the following law in addition to the
  -- | `BoundedLattice` laws:
  -- |
  -- | - Complemented:
  -- |   - `not a || a == top`
  -- |   - `not a && a == bottom`
  -- | - Double negation:
  -- |   - `not <<< not == id`
  class (BoundedLattice a) <= ComplementedLattice a where
    not :: a -> a

  instance complementedLatticeBoolean :: ComplementedLattice Boolean where
    not = boolNot

  instance complementedLatticeUnit :: ComplementedLattice Unit where
    not _ = unit

  -- | The `DistributiveLattice` type class represents types that are lattices
  -- | where the `&&` and `||` distribute over each other.
  -- |
  -- | Instances should satisfy the following law in addition to the `Lattice`
  -- | laws:
  -- |
  -- | - Distributivity: `x && (y || z) = (x && y) || (x && z)`
  class (Lattice a) <= DistributiveLattice a

  instance distributiveLatticeBoolean :: DistributiveLattice Boolean

  instance distributiveLatticeUnit :: DistributiveLattice Unit

  -- | The `BooleanAlgebra` type class represents types that are Boolean
  -- | algebras, also known as Boolean lattices.
  -- |
  -- | Instances should satisfy the `ComplementedLattice` and
  -- | `DistributiveLattice` laws.
  class (ComplementedLattice a, DistributiveLattice a) <= BooleanAlgebra a

  instance booleanAlgebraBoolean :: BooleanAlgebra Boolean

  instance booleanAlgebraUnit :: BooleanAlgebra Unit

  foreign import boolOr
    """
    function boolOr(b1) {
      return function(b2) {
        return b1 || b2;
      };
    }
    """ :: Boolean -> Boolean -> Boolean

  foreign import boolAnd
    """
    function boolAnd(b1) {
      return function(b2) {
        return b1 && b2;
      };
    }
    """  :: Boolean -> Boolean -> Boolean

  foreign import boolNot
    """
    function boolNot(b) {
      return !b;
    }
    """ :: Boolean -> Boolean

  -- | The `Show` type class represents those types which can be converted into
  -- | a human-readable `String` representation.
  -- |
  -- | While not required, it is recommended that for any expression `x`, the
  -- | string `show x` be executable PureScript code which evaluates to the same
  -- | value as the expression `x`.
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

  instance showArray :: (Show a) => Show [a] where
    show = showArrayImpl show

  instance showOrdering :: Show Ordering where
    show LT = "LT"
    show GT = "GT"
    show EQ = "EQ"

  foreign import showNumberImpl
    """
    function showNumberImpl(n) {
      return n.toString();
    }
    """ :: Number -> String

  foreign import showStringImpl
    """
    function showStringImpl(s) {
      return JSON.stringify(s);
    }
    """ :: String -> String

  foreign import showArrayImpl
    """
    function showArrayImpl(f) {
      return function(xs) {
        var ss = [];
        for (var i = 0, l = xs.length; i < l; i++) {
          ss[i] = f(xs[i]);
        }
        return '[' + ss.join(',') + ']';
      };
    }
    """ :: forall a. (a -> String) -> [a] -> String

module Data.Function where

  -- | The `on` function is used to change the domain of a binary operator.
  -- |
  -- | For example, we can create a function which compares two records based on the values of their `x` properties:
  -- |
  -- | ```purescript
  -- | compareX :: forall r. { x :: Number | r } -> { x :: Number | r } -> Ordering
  -- | compareX = compare `on` _.x
  -- | ```
  on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
  on f g x y = g x `f` g y

  -- | A function of zero arguments
  foreign import data Fn0 :: * -> *

  -- | A function of one argument
  foreign import data Fn1 :: * -> * -> *

  -- | A function of two arguments
  foreign import data Fn2 :: * -> * -> * -> *

  -- | A function of three arguments
  foreign import data Fn3 :: * -> * -> * -> * -> *

  -- | A function of four arguments
  foreign import data Fn4 :: * -> * -> * -> * -> * -> *

  -- | A function of five arguments
  foreign import data Fn5 :: * -> * -> * -> * -> * -> * -> *

  -- | A function of six arguments
  foreign import data Fn6 :: * -> * -> * -> * -> * -> * -> * -> *

  -- | A function of seven arguments
  foreign import data Fn7 :: * -> * -> * -> * -> * -> * -> * -> * -> *

  -- | A function of eight arguments
  foreign import data Fn8 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> *

  -- | A function of nine arguments
  foreign import data Fn9 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *

  -- | A function of ten arguments
  foreign import data Fn10 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *

  -- | Create a function of no arguments
  foreign import mkFn0
    """
    function mkFn0(fn) {
      return function() {
        return fn({});
      };
    }
    """ :: forall a. (Unit -> a) -> Fn0 a

  -- | Create a function of one argument
  foreign import mkFn1
    """
    function mkFn1(fn) {
      return function(a) {
        return fn(a);
      };
    }
    """ :: forall a b. (a -> b) -> Fn1 a b

  -- | Create a function of two arguments from a curried function
  foreign import mkFn2
    """
    function mkFn2(fn) {
      return function(a, b) {
        return fn(a)(b);
      };
    }
    """ :: forall a b c. (a -> b -> c) -> Fn2 a b c

  -- | Create a function of three arguments from a curried function
  foreign import mkFn3
    """
    function mkFn3(fn) {
      return function(a, b, c) {
        return fn(a)(b)(c);
      };
    }
    """ :: forall a b c d. (a -> b -> c -> d) -> Fn3 a b c d

  -- | Create a function of four arguments from a curried function
  foreign import mkFn4
    """
    function mkFn4(fn) {
      return function(a, b, c, d) {
        return fn(a)(b)(c)(d);
      };
    }
    """ :: forall a b c d e. (a -> b -> c -> d -> e) -> Fn4 a b c d e

  -- | Create a function of five arguments from a curried function
  foreign import mkFn5
    """
    function mkFn5(fn) {
      return function(a, b, c, d, e) {
        return fn(a)(b)(c)(d)(e);
      };
    }
    """ :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Fn5 a b c d e f

  -- | Create a function of six arguments from a curried function
  foreign import mkFn6
    """
    function mkFn6(fn) {
      return function(a, b, c, d, e, f) {
        return fn(a)(b)(c)(d)(e)(f);
      };
    }
    """ :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Fn6 a b c d e f g

  -- | Create a function of seven arguments from a curried function
  foreign import mkFn7
    """
    function mkFn7(fn) {
      return function(a, b, c, d, e, f, g) {
        return fn(a)(b)(c)(d)(e)(f)(g);
      };
    }
    """ :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> Fn7 a b c d e f g h

  -- | Create a function of eight arguments from a curried function
  foreign import mkFn8
    """
    function mkFn8(fn) {
      return function(a, b, c, d, e, f, g, h) {
        return fn(a)(b)(c)(d)(e)(f)(g)(h);
      };
    }
    """ :: forall a b c d e f g h i. (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Fn8 a b c d e f g h i

  -- | Create a function of nine arguments from a curried function
  foreign import mkFn9
    """
    function mkFn9(fn) {
      return function(a, b, c, d, e, f, g, h, i) {
        return fn(a)(b)(c)(d)(e)(f)(g)(h)(i);
      };
    }
    """ :: forall a b c d e f g h i j. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Fn9 a b c d e f g h i j

  -- | Create a function of ten arguments from a curried function
  foreign import mkFn10
    """
    function mkFn10(fn) {
      return function(a, b, c, d, e, f, g, h, i, j) {
        return fn(a)(b)(c)(d)(e)(f)(g)(h)(i)(j);
      };
    }
    """ :: forall a b c d e f g h i j k. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Fn10 a b c d e f g h i j k

  -- | Apply a function of no arguments
  foreign import runFn0
    """
    function runFn0(fn) {
      return fn();
    }
    """ :: forall a. Fn0 a -> a

  -- | Apply a function of one argument
  foreign import runFn1
    """
    function runFn1(fn) {
      return function(a) {
        return fn(a);
      };
    }
    """ :: forall a b. Fn1 a b -> a -> b

  -- | Apply a function of two arguments
  foreign import runFn2
    """
    function runFn2(fn) {
      return function(a) {
        return function(b) {
          return fn(a, b);
        };
      };
    }
    """ :: forall a b c. Fn2 a b c -> a -> b -> c

  -- | Apply a function of three arguments
  foreign import runFn3
    """
    function runFn3(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return fn(a, b, c);
          };
        };
      };
    }
    """ :: forall a b c d. Fn3 a b c d -> a -> b -> c -> d

  -- | Apply a function of four arguments
  foreign import runFn4
    """
    function runFn4(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return fn(a, b, c, d);
            };
          };
        };
      };
    }
    """ :: forall a b c d e. Fn4 a b c d e -> a -> b -> c -> d -> e

  -- | Apply a function of five arguments
  foreign import runFn5
    """
    function runFn5(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return fn(a, b, c, d, e);
              };
            };
          };
        };
      };
    }
    """ :: forall a b c d e f. Fn5 a b c d e f -> a -> b -> c -> d -> e -> f

  -- | Apply a function of six arguments
  foreign import runFn6
    """
    function runFn6(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return fn(a, b, c, d, e, f);
                };
              };
            };
          };
        };
      };
    }
    """ :: forall a b c d e f g. Fn6 a b c d e f g -> a -> b -> c -> d -> e -> f -> g

  -- | Apply a function of seven arguments
  foreign import runFn7
    """
    function runFn7(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return function(g) {
                    return fn(a, b, c, d, e, f, g);
                  };
                };
              };
            };
          };
        };
      };
    }
    """ :: forall a b c d e f g h. Fn7 a b c d e f g h -> a -> b -> c -> d -> e -> f -> g -> h

  -- | Apply a function of eight arguments
  foreign import runFn8
    """
    function runFn8(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return function(g) {
                    return function(h) {
                      return fn(a, b, c, d, e, f, g, h);
                    };
                  };
                };
              };
            };
          };
        };
      };
    }
    """ :: forall a b c d e f g h i. Fn8 a b c d e f g h i -> a -> b -> c -> d -> e -> f -> g -> h -> i

  -- | Apply a function of nine arguments
  foreign import runFn9
    """
    function runFn9(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return function(g) {
                    return function(h) {
                      return function(i) {
                        return fn(a, b, c, d, e, f, g, h, i);
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    }
    """ :: forall a b c d e f g h i j. Fn9 a b c d e f g h i j -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j

  -- | Apply a function of ten arguments
  foreign import runFn10
    """
    function runFn10(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return function(g) {
                    return function(h) {
                      return function(i) {
                        return function(j) {
                          return fn(a, b, c, d, e, f, g, h, i, j);
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    }
    """ :: forall a b c d e f g h i j k. Fn10 a b c d e f g h i j k -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k

module Prelude.Unsafe where

  -- | Find the element of an array at the specified index.
  -- |
  -- | Note: this function can cause unpredictable failure at runtime if the index is out-of-bounds.
  foreign import unsafeIndex
    """
    function unsafeIndex(xs) {
      return function(n) {
        return xs[n];
      };
    }
    """ :: forall a. [a] -> Number -> a

module Control.Monad.Eff
  ( Eff()
  , Pure()
  , runPure
  , untilE, whileE, forE, foreachE
  ) where

  -- | The `Eff` type constructor is used to represent _native_ effects.
  -- |
  -- | See [Handling Native Effects with the Eff Monad](https://github.com/purescript/purescript/wiki/Handling-Native-Effects-with-the-Eff-Monad) for more details.
  -- |
  -- | The first type parameter is a row of effects which represents the contexts in which a computation can be run, and the second type parameter is the return type.
  foreign import data Eff :: # ! -> * -> *

  foreign import returnE
    """
    function returnE(a) {
      return function() {
        return a;
      };
    }
    """ :: forall e a. a -> Eff e a

  foreign import bindE
    """
    function bindE(a) {
      return function(f) {
        return function() {
          return f(a())();
        };
      };
    }
    """ :: forall e a b. Eff e a -> (a -> Eff e b) -> Eff e b

  -- | The `Pure` type synonym represents _pure_ computations, i.e. ones in which all effects have been handled.
  -- |
  -- | The `runPure` function can be used to run pure computations and obtain their result.
  type Pure a = forall e. Eff e a

  -- | Run a pure computation and return its result.
  -- |
  -- | Note: since this function has a rank-2 type, it may cause problems to apply this function using the `$` operator. The recommended approach
  -- | is to use parentheses instead.
  foreign import runPure
    """
    function runPure(f) {
      return f();
    }
    """ :: forall a. Pure a -> a

  instance functorEff :: Functor (Eff e) where
    (<$>) = liftA1

  instance applyEff :: Apply (Eff e) where
    (<*>) = ap

  instance applicativeEff :: Applicative (Eff e) where
    pure = returnE

  instance bindEff :: Bind (Eff e) where
    (>>=) = bindE

  instance monadEff :: Monad (Eff e)

  -- | Loop until a condition becomes `true`.
  -- |
  -- | `untilE b` is an effectful computation which repeatedly runs the effectful computation `b`,
  -- | until its return value is `true`.
  foreign import untilE
    """
    function untilE(f) {
      return function() {
        while (!f()) {};
        return {};
      };
    }
    """ :: forall e. Eff e Boolean -> Eff e Unit

  -- | Loop while a condition is `true`.
  -- |
  -- | `whileE b m` is effectful computation which runs the effectful computation `b`. If its result is
  -- | `true`, it runs the effectful computation `m` and loops. If not, the computation ends.
  foreign import whileE
    """
    function whileE(f) {
      return function(a) {
        return function() {
          while (f()) {
            a();
          }
          return {};
        };
      };
    }
    """ :: forall e a. Eff e Boolean -> Eff e a -> Eff e Unit

  -- | Loop over a consecutive collection of numbers.
  -- |
  -- | `forE lo hi f` runs the computation returned by the function `f` for each of the inputs
  -- | between `lo` (inclusive) and `hi` (exclusive).
  foreign import forE
    """
    function forE(lo) {
      return function(hi) {
        return function(f) {
          return function() {
            for (var i = lo; i < hi; i++) {
              f(i)();
            }
          };
        };
      };
    }
    """ :: forall e. Number -> Number -> (Number -> Eff e Unit) -> Eff e Unit

  -- | Loop over an array of values.
  -- |
  -- | `foreach xs f` runs the computation returned by the function `f` for each of the inputs `xs`.
  foreign import foreachE
    """
    function foreachE(as) {
      return function(f) {
        return function() {
          for (var i = 0; i < as.length; i++) {
            f(as[i])();
          }
        };
      };
    }
    """ :: forall e a. [a] -> (a -> Eff e Unit) -> Eff e Unit

module Control.Monad.Eff.Unsafe where

  import Control.Monad.Eff

  -- | Change the type of an effectful computation, allowing it to be run in another context.
  -- |
  -- | Note: use of this function can result in arbitrary side-effects.
  foreign import unsafeInterleaveEff
    """
    function unsafeInterleaveEff(f) {
      return f;
    }
    """ :: forall eff1 eff2 a. Eff eff1 a -> Eff eff2 a

module Debug.Trace where

  import Control.Monad.Eff

  -- | The `Trace` effect represents those computations which write to the console.
  foreign import data Trace :: !

  -- | Write a `String` to the console.
  foreign import trace
    """
    function trace(s) {
      return function() {
        console.log(s);
        return {};
      };
    }
    """ :: forall r. String -> Eff (trace :: Trace | r) Unit

  -- | Write a value to the console, using its `Show` instance to produce a `String`.
  print :: forall a r. (Show a) => a -> Eff (trace :: Trace | r) Unit
  print o = trace (show o)

module Control.Monad.ST where

  import Control.Monad.Eff

  -- | The `ST` effect represents _local mutation_, i.e. mutation which does not "escape" into the surrounding computation.
  -- |
  -- | An `ST` computation is parameterized by a phantom type which is used to restrict the set of reference cells it is allowed to access.
  -- |
  -- | The `runST` function can be used to handle the `ST` effect.
  foreign import data ST :: * -> !

  -- | The type `STRef s a` represents a mutable reference holding a value of type `a`, which can be used with the `ST s` effect.
  foreign import data STRef :: * -> * -> *

  -- | Create a new mutable reference.
  foreign import newSTRef
    """
    function newSTRef(val) {
      return function() {
        return { value: val };
      };
    }
    """ :: forall a h r. a -> Eff (st :: ST h | r) (STRef h a)

  -- | Read the current value of a mutable reference.
  foreign import readSTRef
    """
    function readSTRef(ref) {
      return function() {
        return ref.value;
      };
    }
    """ :: forall a h r. STRef h a -> Eff (st :: ST h | r) a

  -- | Modify the value of a mutable reference by applying a function to the current value.
  foreign import modifySTRef
    """
    function modifySTRef(ref) {
      return function(f) {
        return function() {
          return ref.value = f(ref.value);
        };
      };
    }
    """ :: forall a h r. STRef h a -> (a -> a) -> Eff (st :: ST h | r) a

  -- | Set the value of a mutable reference.
  foreign import writeSTRef
    """
    function writeSTRef(ref) {
      return function(a) {
        return function() {
          return ref.value = a;
        };
      };
    }
    """ :: forall a h r. STRef h a -> a -> Eff (st :: ST h | r) a

  -- | Run an `ST` computation.
  -- |
  -- | Note: the type of `runST` uses a rank-2 type to constrain the phantom type `s`, such that the computation must not leak any mutable references
  -- | to the surrounding computation.
  -- |
  -- | It may cause problems to apply this function using the `$` operator. The recommended approach is to use parentheses instead.
  foreign import runST
    """
    function runST(f) {
      return f;
    }
    """ :: forall a r. (forall h. Eff (st :: ST h | r) a) -> Eff r a

  -- | A convenience function which combines `runST` with `runPure`, which can be used when the only required effect is `ST`.
  -- |
  -- | Note: since this function has a rank-2 type, it may cause problems to apply this function using the `$` operator. The recommended approach
  -- | is to use parentheses instead.
  pureST :: forall a. (forall h r. Eff (st :: ST h | r) a) -> a
  pureST st = runPure (runST st)

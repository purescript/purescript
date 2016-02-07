module Prelude
  ( Unit(), unit
  , ($), (#)
  , flip
  , const
  , asTypeOf
  , otherwise
  , Semigroupoid, compose, (<<<), (>>>)
  , Category, id
  , Functor, map, (<$>), (<#>), void
  , Apply, apply, (<*>)
  , Applicative, pure, liftA1
  , Bind, bind, (>>=)
  , Monad, return, liftM1, ap
  , Semigroup, append, (<>), (++)
  , Semiring, add, zero, mul, one, (+), (*)
  , ModuloSemiring, div, mod, (/)
  , Ring, sub, negate, (-)
  , Num
  , DivisionRing
  , Eq, eq, (==), (/=)
  , Ordering(..), Ord, compare, (<), (>), (<=), (>=)
  , unsafeCompare
  , Bounded, top, bottom
  , BoundedOrd
  , BooleanAlgebra, conj, disj, not, (&&), (||)
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
infixl 1 #

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
  compose :: forall b c d. a c d -> a b c -> a b d

instance semigroupoidFn :: Semigroupoid (->) where
  compose f g x = f (g x)

infixr 9 >>>
infixr 9 <<<

-- | `(<<<)` is an alias for `compose`.
(<<<) :: forall a b c d. (Semigroupoid a) => a c d -> a b c -> a b d
(<<<) = compose

-- | Forwards composition, or `(<<<)` with its arguments reversed.
(>>>) :: forall a b c d. (Semigroupoid a) => a b c -> a c d -> a b d
(>>>) = flip compose

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

instance categoryFn :: Category (->) where
  id x = x

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
  map :: forall a b. (a -> b) -> f a -> f b

instance functorFn :: Functor ((->) r) where
  map = compose

instance functorArray :: Functor Array where
  map = arrayMap

foreign import arrayMap :: forall a b. (a -> b) -> Array a -> Array b

infixl 4 <$>
infixl 1 <#>

-- | `(<$>)` is an alias for `map`
(<$>) :: forall f a b. (Functor f) => (a -> b) -> f a -> f b
(<$>) = map

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
  apply :: forall a b. f (a -> b) -> f a -> f b

instance applyFn :: Apply ((->) r) where
  apply f g x = f x (g x)

instance applyArray :: Apply Array where
  apply = ap

infixl 4 <*>

-- | `(<*>)` is an alias for `apply`.
(<*>) :: forall f a b. (Apply f) => f (a -> b) -> f a -> f b
(<*>) = apply

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

instance applicativeFn :: Applicative ((->) r) where
  pure = const

instance applicativeArray :: Applicative Array where
  pure x = [x]

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
-- |   map = liftA1
-- | ```
liftA1 :: forall f a b. (Applicative f) => (a -> b) -> f a -> f b
liftA1 f a = pure f <*> a

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
  bind :: forall a b. m a -> (a -> m b) -> m b

instance bindFn :: Bind ((->) r) where
  bind m f x = f (m x) x

instance bindArray :: Bind Array where
  bind = arrayBind

foreign import arrayBind :: forall a b. Array a -> (a -> Array b) -> Array b

infixl 1 >>=

-- | `(>>=)` is an alias for `bind`.
(>>=) :: forall m a b. (Bind m) => m a -> (a -> m b) -> m b
(>>=) = bind

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

instance monadFn :: Monad ((->) r)
instance monadArray :: Monad Array

-- | `liftM1` provides a default implementation of `(<$>)` for any
-- | [`Monad`](#monad), without using `(<$>)` as provided by the
-- | [`Functor`](#functor)-[`Monad`](#monad) superclass relationship.
-- |
-- | `liftM1` can therefore be used to write [`Functor`](#functor) instances
-- | as follows:
-- |
-- | ```purescript
-- | instance functorF :: Functor F where
-- |   map = liftM1
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
-- |   apply = ap
-- | ```
ap :: forall m a b. (Monad m) => m (a -> b) -> m a -> m b
ap f a = do
  f' <- f
  a' <- a
  return (f' a')

-- | The `Semigroup` type class identifies an associative operation on a type.
-- |
-- | Instances are required to satisfy the following law:
-- |
-- | - Associativity: `(x <> y) <> z = x <> (y <> z)`
-- |
-- | One example of a `Semigroup` is `String`, with `(<>)` defined as string
-- | concatenation.
class Semigroup a where
  append :: a -> a -> a

infixr 5 <>
infixr 5 ++

-- | `(<>)` is an alias for `append`.
(<>) :: forall s. (Semigroup s) => s -> s -> s
(<>) = append

-- | `(++)` is an alternative alias for `append`.
(++) :: forall s. (Semigroup s) => s -> s -> s
(++) = append

instance semigroupString :: Semigroup String where
  append = concatString

instance semigroupUnit :: Semigroup Unit where
  append _ _ = unit

instance semigroupFn :: (Semigroup s') => Semigroup (s -> s') where
  append f g = \x -> f x <> g x

instance semigroupOrdering :: Semigroup Ordering where
  append LT _ = LT
  append GT _ = GT
  append EQ y = y

instance semigroupArray :: Semigroup (Array a) where
  append = concatArray

foreign import concatString :: String -> String -> String
foreign import concatArray :: forall a. Array a -> Array a -> Array a

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
  add  :: a -> a -> a
  zero :: a
  mul  :: a -> a -> a
  one  :: a

instance semiringInt :: Semiring Int where
  add = intAdd
  zero = 0
  mul = intMul
  one = 1

instance semiringNumber :: Semiring Number where
  add = numAdd
  zero = 0.0
  mul = numMul
  one = 1.0

instance semiringUnit :: Semiring Unit where
  add _ _ = unit
  zero = unit
  mul _ _ = unit
  one = unit

infixl 6 +
infixl 7 *

-- | `(+)` is an alias for `add`.
(+) :: forall a. (Semiring a) => a -> a -> a
(+) = add

-- | `(*)` is an alias for `mul`.
(*) :: forall a. (Semiring a) => a -> a -> a
(*) = mul

foreign import intAdd :: Int -> Int -> Int
foreign import intMul :: Int -> Int -> Int
foreign import numAdd :: Number -> Number -> Number
foreign import numMul :: Number -> Number -> Number

-- | The `Ring` class is for types that support addition, multiplication,
-- | and subtraction operations.
-- |
-- | Instances must satisfy the following law in addition to the `Semiring`
-- | laws:
-- |
-- | - Additive inverse: `a - a = (zero - a) + a = zero`
class (Semiring a) <= Ring a where
  sub :: a -> a -> a

instance ringInt :: Ring Int where
  sub = intSub

instance ringNumber :: Ring Number where
  sub = numSub

instance ringUnit :: Ring Unit where
  sub _ _ = unit

infixl 6 -

-- | `(-)` is an alias for `sub`.
(-) :: forall a. (Ring a) => a -> a -> a
(-) = sub

-- | `negate x` can be used as a shorthand for `zero - x`.
negate :: forall a. (Ring a) => a -> a
negate a = zero - a

foreign import intSub :: Int -> Int -> Int
foreign import numSub :: Number -> Number -> Number

-- | The `ModuloSemiring` class is for types that support addition,
-- | multiplication, division, and modulo (division remainder) operations.
-- |
-- | Instances must satisfy the following law in addition to the `Semiring`
-- | laws:
-- |
-- | - Remainder: ``a / b * b + (a `mod` b) = a``
class (Semiring a) <= ModuloSemiring a where
  div :: a -> a -> a
  mod :: a -> a -> a

instance moduloSemiringInt :: ModuloSemiring Int where
  div = intDiv
  mod = intMod

instance moduloSemiringNumber :: ModuloSemiring Number where
  div = numDiv
  mod _ _ = 0.0

instance moduloSemiringUnit :: ModuloSemiring Unit where
  div _ _ = unit
  mod _ _ = unit

infixl 7 /

-- | `(/)` is an alias for `div`.
(/) :: forall a. (ModuloSemiring a) => a -> a -> a
(/) = div

foreign import intDiv :: Int -> Int -> Int
foreign import numDiv :: Number -> Number -> Number
foreign import intMod :: Int -> Int -> Int

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

-- | The `Eq` type class represents types which support decidable equality.
-- |
-- | `Eq` instances should satisfy the following laws:
-- |
-- | - Reflexivity: `x == x = true`
-- | - Symmetry: `x == y = y == x`
-- | - Transitivity: if `x == y` and `y == z` then `x == z`
class Eq a where
  eq :: a -> a -> Boolean

infix 4 ==
infix 4 /=

-- | `(==)` is an alias for `eq`. Tests whether one value is equal to another.
(==) :: forall a. (Eq a) => a -> a -> Boolean
(==) = eq

-- | `(/=)` tests whether one value is _not equal_ to another. Shorthand for
-- | `not (x == y)`.
(/=) :: forall a. (Eq a) => a -> a -> Boolean
(/=) x y = not (x == y)

instance eqBoolean :: Eq Boolean where
  eq = refEq

instance eqInt :: Eq Int where
  eq = refEq

instance eqNumber :: Eq Number where
  eq = refEq

instance eqChar :: Eq Char where
  eq = refEq

instance eqString :: Eq String where
  eq = refEq

instance eqUnit :: Eq Unit where
  eq _ _ = true

instance eqArray :: (Eq a) => Eq (Array a) where
  eq = eqArrayImpl (==)

instance eqOrdering :: Eq Ordering where
  eq LT LT = true
  eq GT GT = true
  eq EQ EQ = true
  eq _  _  = false

foreign import refEq :: forall a. a -> a -> Boolean
foreign import refIneq :: forall a. a -> a -> Boolean
foreign import eqArrayImpl :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Boolean

-- | The `Ordering` data type represents the three possible outcomes of
-- | comparing two values:
-- |
-- | `LT` - The first value is _less than_ the second.
-- | `GT` - The first value is _greater than_ the second.
-- | `EQ` - The first value is _equal to_ the second.
data Ordering = LT | GT | EQ

-- | The `Ord` type class represents types which support comparisons with a
-- | _total order_.
-- |
-- | `Ord` instances should satisfy the laws of total orderings:
-- |
-- | - Reflexivity: `a <= a`
-- | - Antisymmetry: if `a <= b` and `b <= a` then `a = b`
-- | - Transitivity: if `a <= b` and `b <= c` then `a <= c`
class (Eq a) <= Ord a where
  compare :: a -> a -> Ordering

instance ordBoolean :: Ord Boolean where
  compare = unsafeCompare

instance ordInt :: Ord Int where
  compare = unsafeCompare

instance ordNumber :: Ord Number where
  compare = unsafeCompare

instance ordString :: Ord String where
  compare = unsafeCompare

instance ordChar :: Ord Char where
  compare = unsafeCompare

instance ordUnit :: Ord Unit where
  compare _ _ = EQ

instance ordArray :: (Ord a) => Ord (Array a) where
  compare xs ys = compare 0 $ ordArrayImpl (\x y -> case compare x y of
                                                EQ -> 0
                                                LT -> 1
                                                GT -> -1) xs ys

foreign import ordArrayImpl :: forall a. (a -> a -> Int) -> Array a -> Array a -> Int

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

foreign import unsafeCompareImpl :: forall a. Ordering -> Ordering -> Ordering -> a -> a -> Ordering

-- | The `Bounded` type class represents types that are finite.
-- |
-- | Although there are no "internal" laws for `Bounded`, every value of `a`
-- | should be considered less than or equal to `top` by some means, and greater
-- | than or equal to `bottom`.
-- |
-- | The lack of explicit `Ord` constraint allows flexibility in the use of
-- | `Bounded` so it can apply to total and partially ordered sets, boolean
-- | algebras, etc.
class Bounded a where
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

instance boundedInt :: Bounded Int where
  top = topInt
  bottom = bottomInt

-- | Characters fall within the Unicode range.
instance boundedChar :: Bounded Char where
  top = topChar
  bottom = bottomChar

instance boundedFn :: (Bounded b) => Bounded (a -> b) where
  top _ = top
  bottom _ = bottom

foreign import topInt :: Int
foreign import bottomInt :: Int

foreign import topChar :: Char
foreign import bottomChar :: Char

-- | The `BoundedOrd` type class represents totally ordered finite data types.
-- |
-- | Instances should satisfy the following law in addition to the `Ord` laws:
-- |
-- | - Ordering: `bottom <= a <= top`
class (Bounded a, Ord a) <= BoundedOrd a

instance boundedOrdBoolean :: BoundedOrd Boolean where
instance boundedOrdUnit :: BoundedOrd Unit where
instance boundedOrdOrdering :: BoundedOrd Ordering where
instance boundedOrdInt :: BoundedOrd Int where
instance boundedOrdChar :: BoundedOrd Char where

-- | The `BooleanAlgebra` type class represents types that behave like boolean
-- | values.
-- |
-- | Instances should satisfy the following laws in addition to the `Bounded`
-- | laws:
-- |
-- | - Associativity:
-- |   - `a || (b || c) = (a || b) || c`
-- |   - `a && (b && c) = (a && b) && c`
-- | - Commutativity:
-- |   - `a || b = b || a`
-- |   - `a && b = b && a`
-- | - Distributivity:
-- |   - `a && (b || c) = (a && b) || (a && c)`
-- |   - `a || (b && c) = (a || b) && (a || c)`
-- | - Identity:
-- |   - `a || bottom = a`
-- |   - `a && top = a`
-- | - Idempotent:
-- |   - `a || a = a`
-- |   - `a && a = a`
-- | - Absorption:
-- |   - `a || (a && b) = a`
-- |   - `a && (a || b) = a`
-- | - Annhiliation:
-- |   - `a || top = top`
-- | - Complementation:
-- |   - `a && not a = bottom`
-- |   - `a || not a = top`
class (Bounded a) <= BooleanAlgebra a where
  conj :: a -> a -> a
  disj :: a -> a -> a
  not :: a -> a

instance booleanAlgebraBoolean :: BooleanAlgebra Boolean where
  conj = boolAnd
  disj = boolOr
  not = boolNot

instance booleanAlgebraUnit :: BooleanAlgebra Unit where
  conj _ _ = unit
  disj _ _ = unit
  not _ = unit

instance booleanAlgebraFn :: (BooleanAlgebra b) => BooleanAlgebra (a -> b) where
  conj fx fy a = fx a `conj` fy a
  disj fx fy a = fx a `disj` fy a
  not fx a = not (fx a)

infixr 3 &&
infixr 2 ||

-- | `(&&)` is an alias for `conj`.
(&&) :: forall a. (BooleanAlgebra a) => a -> a -> a
(&&) = conj

-- | `(||)` is an alias for `disj`.
(||) :: forall a. (BooleanAlgebra a) => a -> a -> a
(||) = disj

foreign import boolOr :: Boolean -> Boolean -> Boolean
foreign import boolAnd :: Boolean -> Boolean -> Boolean
foreign import boolNot :: Boolean -> Boolean

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

instance showInt :: Show Int where
  show = showIntImpl

instance showNumber :: Show Number where
  show = showNumberImpl

instance showChar :: Show Char where
  show = showCharImpl

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

foreign import showIntImpl :: Int -> String
foreign import showNumberImpl :: Number -> String
foreign import showCharImpl :: Char -> String
foreign import showStringImpl :: String -> String
foreign import showArrayImpl :: forall a. (a -> String) -> Array a -> String

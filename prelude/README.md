# Module Documentation

## Module Prelude

#### `otherwise`

``` purescript
otherwise :: Boolean
```

An alias for `true`, which can be useful in guard clauses:

```purescript
max x y | x >= y = x
        | otherwise = y
```


#### `flip`

``` purescript
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
```

Flips the order of the arguments to a function of two arguments.

```purescript
flip const 1 2 = const 2 1 = 2
```


#### `const`

``` purescript
const :: forall a b. a -> b -> a
```

Returns its first argument and ignores its second.

```purescript
const 1 "hello" = 1
```


#### `asTypeOf`

``` purescript
asTypeOf :: forall a. a -> a -> a
```

This function returns its first argument, and can be used to assert type equalities.
This can be useful when types are otherwise ambiguous.

```purescript
main = print $ [] `asTypeOf` [0]
```

If instead, we had written `main = print []`, the type of the argument `[]` would have
been ambiguous, resulting in a compile-time error.

#### `Semigroupoid`

``` purescript
class Semigroupoid a where
  (<<<) :: forall b c d. a c d -> a b c -> a b d
```

A `Semigroupoid` is similar to a [`Category`](#category) but does not require an identity
element `id`, just composable morphisms.

`Semigroupoid`s should obey the following rule:

- Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`


#### `semigroupoidArr`

``` purescript
instance semigroupoidArr :: Semigroupoid Prim.Function
```


#### `(>>>)`

``` purescript
(>>>) :: forall a b c d. (Semigroupoid a) => a b c -> a c d -> a b d
```


#### `Category`

``` purescript
class (Semigroupoid a) <= Category a where
  id :: forall t. a t t
```

`Category`s consist of objects and composable morphisms between them, and as such are
[`Semigroupoids`](#semigroupoid), but unlike `semigroupoids` must have an identity element.

`Category`s should obey the following rules.

- Left Identity: `id <<< p = p`
- Right Identity: `p <<< id = p`


#### `categoryArr`

``` purescript
instance categoryArr :: Category Prim.Function
```


#### `($)`

``` purescript
($) :: forall a b. (a -> b) -> a -> b
```

Applies a function to its argument

```purescript
length $ groupBy productCategory $ filter isInStock products
```

is equivalent to

```purescript
length (groupBy productCategory (filter isInStock (products)))
```

`($)` is different from [`(#)`](#-2) because it is right-infix instead of left, so
`a $ b $ c $ d x` = `a (b (c (d x)))`


#### `(#)`

``` purescript
(#) :: forall a b. a -> (a -> b) -> b
```

Applies a function to its argument

```purescript
products # groupBy productCategory # filter isInStock # length
```

is equivalent to

```purescript
length (groupBy productCategory (filter isInStock (products)))
```

`(#)` is different from [`($)`](#-1) because it is left-infix instead of right, so
`x # a # b # c # d` = `(((x a) b) c) d`


#### `(:)`

``` purescript
(:) :: forall a. a -> [a] -> [a]
```

Attaches an element to the front of a list.

```purescript
1 : [2, 3, 4] = [1, 2, 3, 4]
```


#### `cons`

``` purescript
cons :: forall a. a -> [a] -> [a]
```


#### `Show`

``` purescript
class Show a where
  show :: a -> String
```


#### `showUnit`

``` purescript
instance showUnit :: Show Unit
```


#### `showString`

``` purescript
instance showString :: Show String
```


#### `showBoolean`

``` purescript
instance showBoolean :: Show Boolean
```


#### `showNumber`

``` purescript
instance showNumber :: Show Number
```


#### `showArray`

``` purescript
instance showArray :: (Show a) => Show [a]
```


#### `Functor`

``` purescript
class Functor f where
  (<$>) :: forall a b. (a -> b) -> f a -> f b
```

A `Functor` is intuitively a type which can be mapped over, and more formally a mapping
between [`Category`](#category)s that preserves structure.

`Functor`s should obey the following rules.

- Identity: `(<$>) id = id`
- Composition: `(<$>) (f <<< g) = (<$> f) <<< (<$> g)`


#### `(<#>)`

``` purescript
(<#>) :: forall f a b. (Functor f) => f a -> (a -> b) -> f b
```


#### `void`

``` purescript
void :: forall f a. (Functor f) => f a -> f Unit
```


#### `Apply`

``` purescript
class (Functor f) <= Apply f where
  (<*>) :: forall a b. f (a -> b) -> f a -> f b
```

`Apply`s are intuitively [`Applicative`](#applicative)s less `pure`, and more formally a
strong lax semi-monoidal endofunctor.

`Apply`s should obey the following rule.

- Associative Composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`


#### `Applicative`

``` purescript
class (Apply f) <= Applicative f where
  pure :: forall a. a -> f a
```

`Applicative`s are [`Functor`](#functor)s which can be "applied" by sequencing composition
(`<*>`) or embedding pure expressions (`pure`).

`Applicative`s should obey the following rules.

- Identity: `(pure id) <*> v = v`
- Composition: `(pure <<<) <*> f <*> g <*> h = f <*> (g <*> h)`
- Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
- Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`


#### `liftA1`

``` purescript
liftA1 :: forall f a b. (Applicative f) => (a -> b) -> f a -> f b
```


#### `Bind`

``` purescript
class (Apply m) <= Bind m where
  (>>=) :: forall a b. m a -> (a -> m b) -> m b
```

A `Bind` is an [`Apply`](#apply) with a bind operation which sequentially composes actions.

`Bind`s should obey the following rule.

- Associativity: `forall f g x. (x >>= f) >>= g = x >>= (\k => f k >>= g)`


#### `Monad`

``` purescript
class (Applicative m, Bind m) <= Monad m where
```

`Monad` is a class which can be intuitively thought of as an abstract datatype of actions or
more formally though of as a monoid in the category of endofunctors.

`Monad`s should obey the following rules.

- Left Identity: `pure x >>= f = f x`
- Right Identity: `x >>= pure = x`


#### `return`

``` purescript
return :: forall m a. (Monad m) => a -> m a
```


#### `liftM1`

``` purescript
liftM1 :: forall m a b. (Monad m) => (a -> b) -> m a -> m b
```


#### `ap`

``` purescript
ap :: forall m a b. (Monad m) => m (a -> b) -> m a -> m b
```


#### `functorArr`

``` purescript
instance functorArr :: Functor (Prim.Function r)
```


#### `applyArr`

``` purescript
instance applyArr :: Apply (Prim.Function r)
```


#### `applicativeArr`

``` purescript
instance applicativeArr :: Applicative (Prim.Function r)
```


#### `bindArr`

``` purescript
instance bindArr :: Bind (Prim.Function r)
```


#### `monadArr`

``` purescript
instance monadArr :: Monad (Prim.Function r)
```


#### `Semiring`

``` purescript
class Semiring a where
  (+) :: a -> a -> a
  zero :: a
  (*) :: a -> a -> a
  one :: a
```

Addition and multiplication, satisfying the following laws:

- `a` is a commutative monoid under addition with identity element zero
- `a` is a monoid under multiplication with identity element one
- multiplication distributes over addition
- multiplication by `zero` annihilates `a`


#### `ModuloSemiring`

``` purescript
class (Semiring a) <= ModuloSemiring a where
  (/) :: a -> a -> a
  mod :: a -> a -> a
```

Addition, multiplication, modulo operation and division, satisfying:

- ```a / b * b + (a `mod` b) = a```


#### `Ring`

``` purescript
class (Semiring a) <= Ring a where
  (-) :: a -> a -> a
```

Addition, multiplication, and subtraction.

Has the same laws as `Semiring` but additionally satisfying:

- `a` is an abelian group under addition


#### `negate`

``` purescript
negate :: forall a. (Ring a) => a -> a
```


#### `DivisionRing`

``` purescript
class (Ring a, ModuloSemiring a) <= DivisionRing a where
```

Ring where every nonzero element has a multiplicative inverse so that:

- ```a `mod` b = zero```


#### `Num`

``` purescript
class (DivisionRing a) <= Num a where
```

A commutative field

#### `semiringNumber`

``` purescript
instance semiringNumber :: Semiring Number
```


#### `ringNumber`

``` purescript
instance ringNumber :: Ring Number
```


#### `moduloSemiringNumber`

``` purescript
instance moduloSemiringNumber :: ModuloSemiring Number
```


#### `divisionRingNumber`

``` purescript
instance divisionRingNumber :: DivisionRing Number
```


#### `numNumber`

``` purescript
instance numNumber :: Num Number
```


#### `Unit`

``` purescript
newtype Unit
  = Unit {  }
```


#### `unit`

``` purescript
unit :: Unit
```


#### `Eq`

``` purescript
class Eq a where
  (==) :: a -> a -> Boolean
  (/=) :: a -> a -> Boolean
```

Class for types that have an equality comparison.

#### `refEq`

``` purescript
refEq :: forall a. a -> a -> Boolean
```


#### `refIneq`

``` purescript
refIneq :: forall a. a -> a -> Boolean
```


#### `eqUnit`

``` purescript
instance eqUnit :: Eq Unit
```


#### `eqString`

``` purescript
instance eqString :: Eq String
```


#### `eqNumber`

``` purescript
instance eqNumber :: Eq Number
```


#### `eqBoolean`

``` purescript
instance eqBoolean :: Eq Boolean
```


#### `eqArray`

``` purescript
instance eqArray :: (Eq a) => Eq [a]
```


#### `Ordering`

``` purescript
data Ordering
  = LT 
  | GT 
  | EQ 
```


#### `eqOrdering`

``` purescript
instance eqOrdering :: Eq Ordering
```


#### `showOrdering`

``` purescript
instance showOrdering :: Show Ordering
```


#### `semigroupOrdering`

``` purescript
instance semigroupOrdering :: Semigroup Ordering
```


#### `Ord`

``` purescript
class (Eq a) <= Ord a where
  compare :: a -> a -> Ordering
```

Class for types that have ordered comparisons.

Represents a partially ordered set satisfying the following laws:

- Reflexivity: `a <= a`
- Antisymmetry: if `a <= b` and `b <= a` then `a = b`
- Transitivity: if `a <= b` and `b <= c` then `a <= c`


#### `(<)`

``` purescript
(<) :: forall a. (Ord a) => a -> a -> Boolean
```


#### `(>)`

``` purescript
(>) :: forall a. (Ord a) => a -> a -> Boolean
```


#### `(<=)`

``` purescript
(<=) :: forall a. (Ord a) => a -> a -> Boolean
```


#### `(>=)`

``` purescript
(>=) :: forall a. (Ord a) => a -> a -> Boolean
```


#### `ordUnit`

``` purescript
instance ordUnit :: Ord Unit
```


#### `ordBoolean`

``` purescript
instance ordBoolean :: Ord Boolean
```


#### `ordNumber`

``` purescript
instance ordNumber :: Ord Number
```


#### `ordString`

``` purescript
instance ordString :: Ord String
```


#### `ordArray`

``` purescript
instance ordArray :: (Ord a) => Ord [a]
```


#### `Bits`

``` purescript
class Bits b where
  (.&.) :: b -> b -> b
  (.|.) :: b -> b -> b
  (.^.) :: b -> b -> b
  shl :: b -> Number -> b
  shr :: b -> Number -> b
  zshr :: b -> Number -> b
  complement :: b -> b
```


#### `bitsNumber`

``` purescript
instance bitsNumber :: Bits Number
```


#### `BoolLike`

``` purescript
class BoolLike b where
  (&&) :: b -> b -> b
  (||) :: b -> b -> b
  not :: b -> b
```


#### `boolLikeBoolean`

``` purescript
instance boolLikeBoolean :: BoolLike Boolean
```


#### `Semigroup`

``` purescript
class Semigroup a where
  (<>) :: a -> a -> a
```


#### `semigroupUnit`

``` purescript
instance semigroupUnit :: Semigroup Unit
```


#### `semigroupString`

``` purescript
instance semigroupString :: Semigroup String
```


#### `semigroupArr`

``` purescript
instance semigroupArr :: (Semigroup s') => Semigroup (s -> s')
```


#### `(++)`

``` purescript
(++) :: forall s. (Semigroup s) => s -> s -> s
```



## Module Data.Function

#### `on`

``` purescript
on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
```


#### `Fn0`

``` purescript
data Fn0 :: * -> *
```


#### `Fn1`

``` purescript
data Fn1 :: * -> * -> *
```


#### `Fn2`

``` purescript
data Fn2 :: * -> * -> * -> *
```


#### `Fn3`

``` purescript
data Fn3 :: * -> * -> * -> * -> *
```


#### `Fn4`

``` purescript
data Fn4 :: * -> * -> * -> * -> * -> *
```


#### `Fn5`

``` purescript
data Fn5 :: * -> * -> * -> * -> * -> * -> *
```


#### `Fn6`

``` purescript
data Fn6 :: * -> * -> * -> * -> * -> * -> * -> *
```


#### `Fn7`

``` purescript
data Fn7 :: * -> * -> * -> * -> * -> * -> * -> * -> *
```


#### `Fn8`

``` purescript
data Fn8 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> *
```


#### `Fn9`

``` purescript
data Fn9 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *
```


#### `Fn10`

``` purescript
data Fn10 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *
```


#### `mkFn0`

``` purescript
mkFn0 :: forall a. (Unit -> a) -> Fn0 a
```


#### `mkFn1`

``` purescript
mkFn1 :: forall a b. (a -> b) -> Fn1 a b
```


#### `mkFn2`

``` purescript
mkFn2 :: forall a b c. (a -> b -> c) -> Fn2 a b c
```


#### `mkFn3`

``` purescript
mkFn3 :: forall a b c d. (a -> b -> c -> d) -> Fn3 a b c d
```


#### `mkFn4`

``` purescript
mkFn4 :: forall a b c d e. (a -> b -> c -> d -> e) -> Fn4 a b c d e
```


#### `mkFn5`

``` purescript
mkFn5 :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Fn5 a b c d e f
```


#### `mkFn6`

``` purescript
mkFn6 :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Fn6 a b c d e f g
```


#### `mkFn7`

``` purescript
mkFn7 :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> Fn7 a b c d e f g h
```


#### `mkFn8`

``` purescript
mkFn8 :: forall a b c d e f g h i. (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Fn8 a b c d e f g h i
```


#### `mkFn9`

``` purescript
mkFn9 :: forall a b c d e f g h i j. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Fn9 a b c d e f g h i j
```


#### `mkFn10`

``` purescript
mkFn10 :: forall a b c d e f g h i j k. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Fn10 a b c d e f g h i j k
```


#### `runFn0`

``` purescript
runFn0 :: forall a. Fn0 a -> a
```


#### `runFn1`

``` purescript
runFn1 :: forall a b. Fn1 a b -> a -> b
```


#### `runFn2`

``` purescript
runFn2 :: forall a b c. Fn2 a b c -> a -> b -> c
```


#### `runFn3`

``` purescript
runFn3 :: forall a b c d. Fn3 a b c d -> a -> b -> c -> d
```


#### `runFn4`

``` purescript
runFn4 :: forall a b c d e. Fn4 a b c d e -> a -> b -> c -> d -> e
```


#### `runFn5`

``` purescript
runFn5 :: forall a b c d e f. Fn5 a b c d e f -> a -> b -> c -> d -> e -> f
```


#### `runFn6`

``` purescript
runFn6 :: forall a b c d e f g. Fn6 a b c d e f g -> a -> b -> c -> d -> e -> f -> g
```


#### `runFn7`

``` purescript
runFn7 :: forall a b c d e f g h. Fn7 a b c d e f g h -> a -> b -> c -> d -> e -> f -> g -> h
```


#### `runFn8`

``` purescript
runFn8 :: forall a b c d e f g h i. Fn8 a b c d e f g h i -> a -> b -> c -> d -> e -> f -> g -> h -> i
```


#### `runFn9`

``` purescript
runFn9 :: forall a b c d e f g h i j. Fn9 a b c d e f g h i j -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j
```


#### `runFn10`

``` purescript
runFn10 :: forall a b c d e f g h i j k. Fn10 a b c d e f g h i j k -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k
```



## Module Prelude.Unsafe

#### `unsafeIndex`

``` purescript
unsafeIndex :: forall a. [a] -> Number -> a
```



## Module Control.Monad.Eff

#### `Eff`

``` purescript
data Eff :: # ! -> * -> *
```


#### `returnE`

``` purescript
returnE :: forall e a. a -> Eff e a
```


#### `bindE`

``` purescript
bindE :: forall e a b. Eff e a -> (a -> Eff e b) -> Eff e b
```


#### `Pure`

``` purescript
type Pure a = forall e. Eff e a
```


#### `runPure`

``` purescript
runPure :: forall a. Pure a -> a
```


#### `functorEff`

``` purescript
instance functorEff :: Functor (Eff e)
```


#### `applyEff`

``` purescript
instance applyEff :: Apply (Eff e)
```


#### `applicativeEff`

``` purescript
instance applicativeEff :: Applicative (Eff e)
```


#### `bindEff`

``` purescript
instance bindEff :: Bind (Eff e)
```


#### `monadEff`

``` purescript
instance monadEff :: Monad (Eff e)
```


#### `untilE`

``` purescript
untilE :: forall e. Eff e Boolean -> Eff e Unit
```


#### `whileE`

``` purescript
whileE :: forall e a. Eff e Boolean -> Eff e a -> Eff e Unit
```


#### `forE`

``` purescript
forE :: forall e. Number -> Number -> (Number -> Eff e Unit) -> Eff e Unit
```


#### `foreachE`

``` purescript
foreachE :: forall e a. [a] -> (a -> Eff e Unit) -> Eff e Unit
```



## Module Control.Monad.Eff.Unsafe

#### `unsafeInterleaveEff`

``` purescript
unsafeInterleaveEff :: forall eff1 eff2 a. Eff eff1 a -> Eff eff2 a
```



## Module Debug.Trace

#### `Trace`

``` purescript
data Trace :: !
```


#### `trace`

``` purescript
trace :: forall r. String -> Eff (trace :: Trace | r) Unit
```


#### `print`

``` purescript
print :: forall a r. (Show a) => a -> Eff (trace :: Trace | r) Unit
```



## Module Control.Monad.ST

#### `ST`

``` purescript
data ST :: * -> !
```


#### `STRef`

``` purescript
data STRef :: * -> * -> *
```


#### `newSTRef`

``` purescript
newSTRef :: forall a h r. a -> Eff (st :: ST h | r) (STRef h a)
```


#### `readSTRef`

``` purescript
readSTRef :: forall a h r. STRef h a -> Eff (st :: ST h | r) a
```


#### `modifySTRef`

``` purescript
modifySTRef :: forall a h r. STRef h a -> (a -> a) -> Eff (st :: ST h | r) a
```


#### `writeSTRef`

``` purescript
writeSTRef :: forall a h r. STRef h a -> a -> Eff (st :: ST h | r) a
```


#### `runST`

``` purescript
runST :: forall a r. (forall h. Eff (st :: ST h | r) a) -> Eff r a
```


#### `pureST`

``` purescript
pureST :: forall a. (forall h r. Eff (st :: ST h | r) a) -> a
```





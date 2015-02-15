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

  -- | An alias for `true`, which can be useful in guard clauses: 
  -- | 
  -- | E.g.
  -- | 
  -- |     max x y | x >= y = x 
  -- |             | otherwise = y
  otherwise :: Boolean
  otherwise = true

  -- | Flips the order of the arguments to a function of two arguments. 
  -- |
  -- | `flip (/) 6 3 == 3 / 6`
  flip :: forall a b c. (a -> b -> c) -> b -> a -> c
  flip f b a = f a b

  -- | Returns its first argument and ignores its second. 
  -- |
  -- | `const 7 "whatever" == 7`
  const :: forall a b. a -> b -> a
  const a _ = a

  -- | This function returns its first argument, and can be used to assert type equalities.
  -- | This can be useful when types are otherwise ambiguous. 
  -- | 
  -- | E.g.
  -- | 
  -- |     main = print $ [] `asTypeOf` [0]
  -- |
  -- | If instead, we had written `main = print []`, the type of the argument `[]` would have
  -- | been ambiguous, resulting in a compile-time error.
  asTypeOf :: forall a. a -> a -> a
  asTypeOf x _ = x

  infixr 9 >>>
  infixr 9 <<<

  class Semigroupoid a where
    (<<<) :: forall b c d. a c d -> a b c -> a b d

  -- | Function composition in right to left sense.
  -- |
  -- | `(f <<< g) x == f (g x)`
  instance semigroupoidArr :: Semigroupoid (->) where
    (<<<) f g x = f (g x)

  -- | Function composition in left to right sense.
  -- |
  -- | `(f >>> g) x == g (f x)`
  (>>>) :: forall a b c d. (Semigroupoid a) => a b c -> a c d -> a b d
  (>>>) f g = g <<< f

  class (Semigroupoid a) <= Category a where
    id :: forall t. a t t

  instance categoryArr :: Category (->) where
    id x = x

  infixr 0 $
  infixl 0 #

  -- | Applies the first argument to the second, making a backward pipe.
  -- |
  -- | `reverse $ "str" ++ "ing" == "gnirts"`
  ($) :: forall a b. (a -> b) -> a -> b
  ($) f x = f x

  -- | Applies the second argument to the first, making a forwards pipe.
  -- |
  -- | `"str" ++ "ing" # reverse == "gnirts"`
  (#) :: forall a b. a -> (a -> b) -> b
  (#) x f = f x

  infixr 6 :

  -- | Prepends an element to a list.
  -- |
  -- | `"try" : ["purescript", "today"] == ["try", "purescript", "today"]`
  (:) :: forall a. a -> [a] -> [a]
  (:) = cons

  foreign import cons
    """
    function cons(e) {
      return function(l) {
        return [e].concat(l);
      };
    }
    """ :: forall a. a -> [a] -> [a]

  -- | Converts data to a String.
  -- |
  -- | `show 1 == "1"`
  class Show a where
    show :: a -> String

  foreign import showStringImpl
    """
    function showStringImpl(s) {
      return JSON.stringify(s);
    }
    """ :: String -> String

  -- | `show (Unit {}) == "Unit {}"`
  instance showUnit :: Show Unit where
    show (Unit {}) = "Unit {}"

  -- | `show "abc" == "abc"`
  instance showString :: Show String where
    show = showStringImpl

  -- | `show true == "true"`
  instance showBoolean :: Show Boolean where
    show true = "true"
    show false = "false"

  foreign import showNumberImpl
    """
    function showNumberImpl(n) {
      return n.toString();
    }
    """ :: Number -> String

  -- | `show 1 == "1"`
  instance showNumber :: Show Number where
    show = showNumberImpl

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

  -- | `show [1, 2, 3] == "[1, 2, 3]"`
  instance showArray :: (Show a) => Show [a] where
    show = showArrayImpl show

  infixl 4 <$>
  infixl 1 <#>

  -- | Apply a function to the data contained in a Functor type, returning a
  -- | new wrapped result.
  -- | 
  -- | `not <$> Just true == Just false`
  class Functor f where
    (<$>) :: forall a b. (a -> b) -> f a -> f b

  -- | Reversed version of (<$>).
  -- |
  -- | `Just true <#> not == Just false`
  (<#>) :: forall f a b. (Functor f) => f a -> (a -> b) -> f b
  (<#>) fa f = f <$> fa

  -- | Replace the contents of a Functor with Unit.
  -- |
  -- | `void (Just false) == Just unit`
  void :: forall f a. (Functor f) => f a -> f Unit
  void fa = const unit <$> fa

  infixl 4 <*>

  -- | Pass arguments into a Functor-wrapped function, returning a new wrapped
  -- | result.
  -- |
  -- | `Just (+) <*> Just 5 <*> Nothing == Nothing`
  class (Functor f) <= Apply f where
    (<*>) :: forall a b. f (a -> b) -> f a -> f b

  class (Apply f) <= Applicative f where
    pure :: forall a. a -> f a

  -- | Apply a function to the data contained in an Applicative type, returning
  -- | a new Applicative result.
  -- |
  -- | `liftA1 not (Just true) == Just false`
  liftA1 :: forall f a b. (Applicative f) => (a -> b) -> f a -> f b
  liftA1 f a = pure f <*> a

  infixl 1 >>=

  -- | Compose monadic actions together, passing the result of one as the 
  -- | input to the next.
  -- |
  class (Apply m) <= Bind m where
    (>>=) :: forall a b. m a -> (a -> m b) -> m b

  class (Applicative m, Bind m) <= Monad m

  return :: forall m a. (Monad m) => a -> m a
  return = pure

  -- | Apply a function to the data in a Monad type, returning a new Monad 
  -- | result.
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

  -- | Perform arithmetic-like operations on data.
  class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    (/) :: a -> a -> a
    (%) :: a -> a -> a
    negate :: a -> a

  foreign import numAdd
    """
    function numAdd(n1) {
      return function(n2) {
        return n1 + n2;
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

  foreign import numMod
    """
    function numMod(n1) {
      return function(n2) {
        return n1 % n2;
      };
    }
    """ :: Number -> Number -> Number

  foreign import numNegate
    """
    function numNegate(n) {
      return -n;
    }
    """ :: Number -> Number

  -- | Standard arithmetic operations on Numbers. `%` is modulo.
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

  -- | Determine whether two data of the same type are equal.
  class Eq a where
    (==) :: a -> a -> Boolean
    (/=) :: a -> a -> Boolean

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

  instance eqUnit :: Eq Unit where
    (==) (Unit {}) (Unit {}) = true
    (/=) (Unit {}) (Unit {}) = false

  -- | Determine whether two Strings are equal.
  -- |
  -- | `"string" == "string"`
  instance eqString :: Eq String where
    (==) = refEq
    (/=) = refIneq

  -- | Determine whether two Numbers are equal.
  -- |
  -- | `1 == 1`
  instance eqNumber :: Eq Number where
    (==) = refEq
    (/=) = refIneq

  -- | Determine whether two Booleans are equal.
  -- |
  -- | `true /= false`
  instance eqBoolean :: Eq Boolean where
    (==) = refEq
    (/=) = refIneq

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

  -- | Determine whether two Arrays are equal.
  -- |
  -- | `[1, 2, 3] == [1, 2, 3]`
  instance eqArray :: (Eq a) => Eq [a] where
    (==) xs ys = eqArrayImpl (==) xs ys
    (/=) xs ys = not (xs == ys)

  -- | Represents the result of a comparison operation in which one item being
  -- | compared can be less than, greater than, or equal to the other.
  data Ordering = LT | GT | EQ

  -- | Determine whether two Orderings are equal.
  -- |
  -- | `compare 1 2 == compare 1 100`
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

  -- | Compare the ranking of two data of the same type.
  -- |
  -- | `compare 1 10 == LT`
  class (Eq a) <= Ord a where
    compare :: a -> a -> Ordering

  infixl 4 <
  
  -- | Returns true if the left argument is "less than" the right argument.
  -- | 
  -- | `1 < 10 == true`
  (<) :: forall a. (Ord a) => a -> a -> Boolean
  (<) a1 a2 = case a1 `compare` a2 of
    LT -> true
    _ -> false

  infixl 4 >

  -- | Returns true if the left argument is "greater than" the right argument.
  -- |
  -- | `1 > 10 == false`
  (>) :: forall a. (Ord a) => a -> a -> Boolean
  (>) a1 a2 = case a1 `compare` a2 of
    GT -> true
    _ -> false

  infixl 4 <=

  -- | Returns true if the left argument is "greater than or equal to" the 
  -- | right argument.
  -- |
  -- | `1 <= 1 == true`
  (<=) :: forall a. (Ord a) => a -> a -> Boolean
  (<=) a1 a2 = case a1 `compare` a2 of
    GT -> false
    _ -> true

  infixl 4 >=
  
  -- | Returns true if the left argument is "less than or equal to" the 
  -- | right argument.
  -- |
  -- | `1 >= 1 == true`
  (>=) :: forall a. (Ord a) => a -> a -> Boolean
  (>=) a1 a2 = case a1 `compare` a2 of
    LT -> false
    _ -> true

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

  unsafeCompare :: forall a. a -> a -> Ordering
  unsafeCompare = unsafeCompareImpl LT EQ GT

  -- | Compare two Units.  Always returns `EQ`.
  -- |
  -- | `compare unit unit == true`
  instance ordUnit :: Ord Unit where
    compare (Unit {}) (Unit {}) = EQ

  -- | Compare two Booleans.  True is greater than false.
  -- |
  -- | `compare false true == LT`
  instance ordBoolean :: Ord Boolean where
    compare false false = EQ
    compare false true  = LT
    compare true  true  = EQ
    compare true  false = GT

  -- | Compare the value of two Numbers.
  -- |
  -- | `compare 1 100 == LT`
  instance ordNumber :: Ord Number where
    compare = unsafeCompare

  -- | Compare Strings ranked alphabetically (by character value).
  -- | 
  -- | `compare "alphabetically" "bob" == LT`
  instance ordString :: Ord String where
    compare = unsafeCompare

  -- | Compare the contents of two Arrays of the same type.  The first element
  -- | of the left array is compared to the first argument of the right array, 
  -- | second element with second element, etc, until the two compared elements
  -- | are not equal.  If one array is a prefix of the other, the shorter array
  -- | is less than the longer one.
  -- |
  -- | `compare [1, 2, 3] [5] == LT`
  -- | `compare [1, 2, 3] [1, 2, 3, 4] == LT`
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

  -- | Bitwise operations on data.
  class Bits b where
    (.&.) :: b -> b -> b
    (.|.) :: b -> b -> b
    (.^.) :: b -> b -> b
    shl :: b -> Number -> b
    shr :: b -> Number -> b
    zshr :: b -> Number -> b
    complement :: b -> b

  foreign import numShl
    """
    function numShl(n1) {
      return function(n2) {
        return n1 << n2;
      };
    }
    """ :: Number -> Number -> Number

  foreign import numShr
    """
    function numShr(n1) {
      return function(n2) {
        return n1 >> n2;
      };
    }
    """ :: Number -> Number -> Number

  foreign import numZshr
    """
    function numZshr(n1) {
      return function(n2) {
        return n1 >>> n2;
      };
    }
    """ :: Number -> Number -> Number

  foreign import numAnd
    """
    function numAnd(n1) {
      return function(n2) {
        return n1 & n2;
      };
    }
    """ :: Number -> Number -> Number

  foreign import numOr
    """
    function numOr(n1) {
      return function(n2) {
        return n1 | n2;
      };
    }
    """ :: Number -> Number -> Number

  foreign import numXor
    """
    function numXor(n1) {
      return function(n2) {
        return n1 ^ n2;
      };
    }
    """ :: Number -> Number -> Number

  foreign import numComplement
    """
    function numComplement(n) {
      return ~n;
    }
    """ :: Number -> Number

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

  -- | Perform Boolean logic operators on data.
  class BoolLike b where
    (&&) :: b -> b -> b
    (||) :: b -> b -> b
    not :: b -> b

  foreign import boolAnd
    """
    function boolAnd(b1) {
      return function(b2) {
        return b1 && b2;
      };
    }
    """  :: Boolean -> Boolean -> Boolean

  foreign import boolOr
    """
    function boolOr(b1) {
      return function(b2) {
        return b1 || b2;
      };
    }
    """ :: Boolean -> Boolean -> Boolean

  foreign import boolNot
    """
    function boolNot(b) {
      return !b;
    }
    """ :: Boolean -> Boolean

  -- | Standard boolean AND and OR.
  -- |
  -- | `true && false == false`
  -- | `true || false == true`
  instance boolLikeBoolean :: BoolLike Boolean where
    (&&) = boolAnd
    (||) = boolOr
    not = boolNot

  infixr 5 <>

  -- | Concatenation-like operation.
  class Semigroup a where
    (<>) :: a -> a -> a

  foreign import concatString
    """
    function concatString(s1) {
      return function(s2) {
        return s1 + s2;
      };
    }
    """ :: String -> String -> String

  instance semigroupUnit :: Semigroup Unit where
    (<>) (Unit {}) (Unit {}) = Unit {}

  -- | Concatenate two Strings.
  -- |
  -- | `"try" <> " purescript" == "try purescript"
  instance semigroupString :: Semigroup String where
    (<>) = concatString

  instance semigroupArr :: (Semigroup s') => Semigroup (s -> s') where
    (<>) f g = \x -> f x <> g x

  infixr 5 ++

  -- | Alias for (<>), typically used for String or Array concatenation as a
  -- | style choice.
  -- |
  -- | `[1, 2, 3] ++ [4, 5] == [1, 2, 3, 4, 5]`
  (++) :: forall s. (Semigroup s) => s -> s -> s
  (++) = (<>)

module Data.Function where

  -- | Apply a function to two arguments, then combine the results with a second
  -- | function.
  -- |
  -- | `on (*) length "argument1" "arg2" == 9 * 4`
  on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
  on f g x y = g x `f` g y

  -- | Function of 0 arguments using Javascript calling conventions.
  foreign import data Fn0 :: * -> *
  -- | Function of 1 arguments using Javascript calling conventions.
  foreign import data Fn1 :: * -> * -> *
  -- | Function of 2 arguments using Javascript calling conventions.
  foreign import data Fn2 :: * -> * -> * -> *
  foreign import data Fn3 :: * -> * -> * -> * -> *
  foreign import data Fn4 :: * -> * -> * -> * -> * -> *
  foreign import data Fn5 :: * -> * -> * -> * -> * -> * -> *
  foreign import data Fn6 :: * -> * -> * -> * -> * -> * -> * -> *
  foreign import data Fn7 :: * -> * -> * -> * -> * -> * -> * -> * -> *
  foreign import data Fn8 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> *
  foreign import data Fn9 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *
  foreign import data Fn10 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *

  -- | Convert a purescript function of type `Unit -> a` to JS calling
  -- | convention.
  foreign import mkFn0
    """
    function mkFn0(fn) {
      return function() {
        return fn({});
      };
    }
    """ :: forall a. (Unit -> a) -> Fn0 a

  -- | Convert a purescript function of 1 argument to JS calling convention.
  foreign import mkFn1
    """
    function mkFn1(fn) {
      return function(a) {
        return fn(a);
      };
    }
    """ :: forall a b. (a -> b) -> Fn1 a b

  -- | Convert a purescript function of 2 argument to JS calling convention.
  foreign import mkFn2
    """
    function mkFn2(fn) {
      return function(a, b) {
        return fn(a)(b);
      };
    }
    """ :: forall a b c. (a -> b -> c) -> Fn2 a b c

  foreign import mkFn3
    """
    function mkFn3(fn) {
      return function(a, b, c) {
        return fn(a)(b)(c);
      };
    }
    """ :: forall a b c d. (a -> b -> c -> d) -> Fn3 a b c d

  foreign import mkFn4
    """
    function mkFn4(fn) {
      return function(a, b, c, d) {
        return fn(a)(b)(c)(d);
      };
    }
    """ :: forall a b c d e. (a -> b -> c -> d -> e) -> Fn4 a b c d e

  foreign import mkFn5
    """
    function mkFn5(fn) {
      return function(a, b, c, d, e) {
        return fn(a)(b)(c)(d)(e);
      };
    }
    """ :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Fn5 a b c d e f

  foreign import mkFn6
    """
    function mkFn6(fn) {
      return function(a, b, c, d, e, f) {
        return fn(a)(b)(c)(d)(e)(f);
      };
    }
    """ :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Fn6 a b c d e f g

  foreign import mkFn7
    """
    function mkFn7(fn) {
      return function(a, b, c, d, e, f, g) {
        return fn(a)(b)(c)(d)(e)(f)(g);
      };
    }
    """ :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> Fn7 a b c d e f g h

  foreign import mkFn8
    """
    function mkFn8(fn) {
      return function(a, b, c, d, e, f, g, h) {
        return fn(a)(b)(c)(d)(e)(f)(g)(h);
      };
    }
    """ :: forall a b c d e f g h i. (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Fn8 a b c d e f g h i

  foreign import mkFn9
    """
    function mkFn9(fn) {
      return function(a, b, c, d, e, f, g, h, i) {
        return fn(a)(b)(c)(d)(e)(f)(g)(h)(i);
      };
    }
    """ :: forall a b c d e f g h i j. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Fn9 a b c d e f g h i j

  foreign import mkFn10
    """
    function mkFn10(fn) {
      return function(a, b, c, d, e, f, g, h, i, j) {
        return fn(a)(b)(c)(d)(e)(f)(g)(h)(i)(j);
      };
    }
    """ :: forall a b c d e f g h i j k. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Fn10 a b c d e f g h i j k

  -- | Run a function with 0 arguments and Javascript calling conventions 
  -- | in purescript.  Useful for writing JS function bindings.
  foreign import runFn0
    """
    function runFn0(fn) {
      return fn();
    }
    """ :: forall a. Fn0 a -> a

  -- | Run a function with 1 argument and Javascript calling conventions 
  -- | in purescript.  Useful for writing JS function bindings.
  foreign import runFn1
    """
    function runFn1(fn) {
      return function(a) {
        return fn(a);
      };
    }
    """ :: forall a b. Fn1 a b -> a -> b

  -- | Run a function with 2 arguments and Javascript calling conventions 
  -- | in purescript.  Useful for writing JS function bindings.
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

  -- | Index into an Array.  No bounds checking is performed on the index.
  foreign import unsafeIndex
    """
    function unsafeIndex(xs) {
      return function(n) {
        return xs[n];
      };
    }
    """ :: forall a. [a] -> Number -> a

module Control.Monad.Eff where

  -- | Monad for executing native Javascript effects.
  foreign import data Eff :: # ! -> * -> *

  -- | Wrap a pure value into an effectful context.
  foreign import returnE
    """
    function returnE(a) {
      return function() {
        return a;
      };
    }
    """ :: forall e a. a -> Eff e a

  -- | Compose effectful actions, passing the result of the first as the input
  -- | to the second.
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

  type Pure a = forall e. Eff e a

  -- | Run an effectful computation with no side effects, either because there 
  -- | are none or because all possible side effects are handled.
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

  foreign import untilE
    """
    function untilE(f) {
      return function() {
        while (!f());
        return {};
      };
    }
    """ :: forall e. Eff e Boolean -> Eff e Unit

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

  foreign import unsafeInterleaveEff
    """
    function unsafeInterleaveEff(f) {
      return f;
    }
    """ :: forall eff1 eff2 a. Eff eff1 a -> Eff eff2 a

module Debug.Trace where

  import Control.Monad.Eff

  foreign import data Trace :: !

  foreign import trace
    """
    function trace(s) {
      return function() {
        console.log(s);
        return {};
      };
    }
    """ :: forall r. String -> Eff (trace :: Trace | r) Unit

  print :: forall a r. (Show a) => a -> Eff (trace :: Trace | r) Unit
  print o = trace (show o)

module Control.Monad.ST where

  import Control.Monad.Eff

  foreign import data ST :: * -> !

  foreign import data STRef :: * -> * -> *

  foreign import newSTRef
    """
    function newSTRef(val) {
      return function() {
        return { value: val };
      };
    }
    """ :: forall a h r. a -> Eff (st :: ST h | r) (STRef h a)

  foreign import readSTRef
    """
    function readSTRef(ref) {
      return function() {
        return ref.value;
      };
    }
    """ :: forall a h r. STRef h a -> Eff (st :: ST h | r) a

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

  foreign import runST
    """
    function runST(f) {
      return f;
    }
    """ :: forall a r. (forall h. Eff (st :: ST h | r) a) -> Eff r a

  pureST :: forall a. (forall h r. Eff (st :: ST h | r) a) -> a
  pureST st = runPure (runST st)

module Prelude where

  flip :: forall a b c. (a -> b -> c) -> b -> a -> c
  flip f b a = f a b

  const :: forall a b. a -> b -> a
  const a _ = a

  infixr 9 >>>
  infixr 9 <<<

  class Category a where
    id :: forall t. a t t
    (<<<) :: forall b c d. a c d -> a b c -> a b d
  
  (>>>) :: forall a b c d. (Category a) => a b c -> a c d -> a b d
  (>>>) f g = g <<< f

  instance categoryArr :: Category (->) where
    id x = x
    (<<<) f g x = f (g x)

  infixr 0 $
  infixl 0 #

  ($) :: forall a b. (a -> b) -> a -> b
  ($) f x = f x

  (#) :: forall a b. a -> (a -> b) -> b
  (#) x f = f x

  class Show a where
    show :: a -> String

  instance showString :: Show String where
    show s = s

  instance showBoolean :: Show Boolean where
    show true = "true"
    show false = "false"

  foreign import showNumberImpl "function showNumberImpl(n) {\
                                \  return n.toString();\
                                \}" :: Number -> String

  instance showNumber :: Show Number where
    show = showNumberImpl

  infixl 4 <$>

  class Functor f where
    (<$>) :: forall a b. (a -> b) -> f a -> f b

  infixl 4 <*>

  class Applicative f where
    pure :: forall a. a -> f a
    (<*>) :: forall a b. f (a -> b) -> f a -> f b

  instance functorFromApplicative :: (Applicative f) => Functor f where
    (<$>) f a = pure f <*> a

  infixl 3 <|>

  class Alternative f where
    empty :: forall a. f a
    (<|>) :: forall a. f a -> f a -> f a

  infixl 1 >>=

  class Monad m where
    return :: forall a. a -> m a
    (>>=) :: forall a b. m a -> (a -> m b) -> m b

  instance applicativeFromMonad :: (Monad m) => Applicative m where
    pure = return
    (<*>) f a = do
      f' <- f
      a' <- a
      return (f' a')

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

  infixl 4 ==
  infixl 4 /=

  class Eq a where
    (==) :: a -> a -> Boolean
    (/=) :: a -> a -> Boolean

  foreign import unsafeRefEq "function unsafeRefEq(r1) {\
                             \  return function(r2) {\
                             \    return r1 === r2;\
                             \  };\
                             \}" :: forall a. a -> a -> Boolean

  foreign import unsafeRefIneq "function unsafeRefIneq(r1) {\
                               \  return function(r2) {\
                               \    return r1 !== r2;\
                               \  };\
                               \}" :: forall a. a -> a -> Boolean

  instance eqString :: Eq String where
    (==) = unsafeRefEq
    (/=) = unsafeRefIneq

  instance eqNumber :: Eq Number where
    (==) = unsafeRefEq
    (/=) = unsafeRefIneq

  instance eqBoolean :: Eq Boolean where
    (==) = unsafeRefEq
    (/=) = unsafeRefIneq

  instance eqArray :: (Eq a) => Eq [a] where
    (==) [] [] = true
    (==) (x:xs) (y:ys) = x == y && xs == ys
    (==) _ _ = false
    (/=) xs ys = not (xs == ys)

  data Ordering = LT | GT | EQ

  instance showOrdering :: Show Ordering where
    show LT = "LT"
    show GT = "GT"
    show EQ = "EQ"
 
  class Ord a where
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

  foreign import numCompare
    "function numCompare(n1) {\
    \  return function(n2) {\
    \    return n1 < n2 ? module.LT : n1 > n2 ? module.GT : module.EQ;\
    \  };\
    \}" :: Number -> Number -> Ordering

  instance ordNumber :: Ord Number where
    compare = numCompare

  infixl 10 &
  infixl 10 |
  infixl 10 ^

  class Bits b where
    (&) :: b -> b -> b
    (|) :: b -> b -> b
    (^) :: b -> b -> b
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
    (&) = numAnd
    (|) = numOr
    (^) = numXor
    shl = numShl
    shr = numShr
    zshr = numZshr
    complement = numComplement

  infixl 8 !!

  foreign import (!!) "function $bang$bang(xs) {\
                      \  return function(n) {\
                      \    return xs[n];\
                      \  };\
                      \}" :: forall a. [a] -> Number -> a

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

  infixr 5 ++

  foreign import (++) "function $plus$plus(s1) {\
                      \  return function(s2) {\
                      \    return s1 + s2;\
                      \  };\
                      \}" :: String -> String -> String

module Data.Monoid where

  import Prelude

  infixr 6 <>

  class Monoid m where
    mempty :: m
    (<>) :: m -> m -> m

  instance monoidString :: Monoid String where
    mempty = ""
    (<>) = (++)

  instance monoidArray :: Monoid [a] where
    mempty = []
    (<>) = Data.Array.concat

module Control.Applicative where

  import Prelude

  infixl 4 <*
  infixl 4 *>

  (<*) :: forall a b f. (Applicative f) => f a -> f b -> f a
  (<*) x y = const <$> x <*> y

  (*>) :: forall a b f. (Applicative f) => f a -> f b -> f b
  (*>) x y = const id <$> x <*> y

  lift2 :: forall a b c f. (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
  lift2 f x y = f <$> x <*> y

  lift3 :: forall a b c d f. (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
  lift3 f x y z = f <$> x <*> y <*> z

module Control.Monad where

  import Prelude
  import Data.Array
  import Data.Traversable

  replicateM :: forall m a. (Monad m) => Number -> m a -> m [a]
  replicateM 0 _ = return []
  replicateM n m = do
    a <- m
    as <- replicateM (n - 1) m
    return (a : as)

  infixr 1 >=>
  infixr 1 <=<

  (>=>) :: forall m a b c. (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
  (>=>) f g a = do
    b <- f a
    g b

  (<=<) :: forall m a b c. (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
  (<=<) = flip (>=>)

  join :: forall m a. (Monad m) => m (m a) -> m a
  join mm = do
    m <- mm
    m

  foldM :: forall m a b. (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
  foldM _ a [] = return a
  foldM f a (b:bs) = f a b >>= \a' -> foldM f a' bs

  when :: forall m. (Monad m) => Boolean -> m {} -> m {}
  when true m = m
  when false _ = return {}

  zipWithA :: forall m a b c. (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
  zipWithA f xs ys = sequence (zipWith f xs ys)

module Data.Maybe where

  import Prelude

  data Maybe a = Nothing | Just a

  maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
  maybe b _ Nothing = b
  maybe _ f (Just a) = f a

  fromMaybe :: forall a. a -> Maybe a -> a
  fromMaybe a = maybe a (id :: forall a. a -> a)

  instance monadMaybe :: Monad Maybe where
    return = Just
    (>>=) m f = maybe Nothing f m

  instance applicativeMaybe :: Applicative Maybe where
    pure = Just
    (<*>) (Just fn) x = fn <$> x
    (<*>) Nothing _ = Nothing

  instance functorMaybe :: Functor Maybe where
    (<$>) fn (Just x) = Just (fn x)
    (<$>) _ _ = Nothing

  instance showMaybe :: (Show a) => Show (Maybe a) where
    show (Just x) = "Just " ++ (show x)
    show Nothing = "Nothing"

  instance eqMaybe :: (Eq a) => Eq (Maybe a) where
    (==) Nothing Nothing = true
    (==) (Just a1) (Just a2) = a1 == a2
    (==) _ _ = false
    (/=) a b = not (a == b)

module Data.Either where

  import Prelude

  data Either a b = Left a | Right b

  either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
  either f _ (Left a) = f a
  either _ g (Right b) = g b

  instance monadEither :: Monad (Either e) where
    return = Right
    (>>=) = either (\e _ -> Left e) (\a f -> f a)

  instance applicativeEither :: Applicative (Either e) where
    pure = Right
    (<*>) (Left e) _ = Left e
    (<*>) (Right f) r = f <$> r

  instance functorEither :: Functor (Either a) where
    (<$>) _ (Left x) = Left x
    (<$>) f (Right y) = Right (f y)

  instance showEither :: (Show a, Show b) => Show (Either a b) where
    show (Left x) = "Left " ++ (show x)
    show (Right y) = "Right " ++ (show y)

  instance eqEither :: (Eq a, Eq b) => Eq (Either a b) where
    (==) (Left a1) (Left a2) = a1 == a2
    (==) (Right b1) (Right b2) = b1 == b2
    (==) _ _ = false
    (/=) a b = not (a == b)

module Data.Array where

  import Prelude
  import Data.Maybe

  head :: forall a. [a] -> Maybe a
  head (x : _) = Just x
  head _ = Nothing

  tail :: forall a. [a] -> Maybe [a]
  tail (_ : xs) = Just xs
  tail _ = Nothing

  map :: forall a b. (a -> b) -> [a] -> [b]
  map _ [] = []
  map f (x:xs) = f x : map f xs

  foreign import length "function length(xs) {\
                        \  return xs.length;\
                        \}" :: forall a. [a] -> Number

  foreign import indexOf "function indexOf(l) {\
                         \  return function (e) {\
                         \    return l.indexOf(e);\
                         \  };\
                         \}" :: forall a. [a] -> a -> Number

  foreign import lastIndexOf "function lastIndexOf(l) {\
                             \  return function (e) {\
                             \    return l.lastIndexOf(e);\
                             \  };\
                             \}" :: forall a. [a] -> a -> Number

  foreign import concat "function concat(l1) {\
                        \  return function (l2) {\
                        \    return l1.concat(l2);\
                        \  };\
                        \}" :: forall a. [a] -> [a] -> [a]

  foreign import joinS "function joinS(l) {\
                       \  return l.join();\
                       \}" :: [String] -> String

  foreign import joinWith "function joinWith(l) {\
                          \  return function (s) {\
                          \    return l.join(s);\
                          \  };\
                          \}" :: [String] -> String -> String

  foreign import push "function push(l) {\
                      \  return function (e) {\
                      \    var l1 = l.slice();\
                      \    l1.push(e); \
                      \    return l1;\
                      \  };\
                      \}" :: forall a. [a] -> a -> [a]

  foreign import reverse "function reverse(l) {\
                         \  var l1 = l.slice();\
                         \  l1.reverse(); \
                         \  return l1;\
                         \}" :: forall a. [a] -> [a]

  foreign import shift "function shift(l) {\
                       \  var l1 = l.slice();\
                       \  l1.shift();\
                       \  return l1;\
                       \}" :: forall a. [a] -> [a]

  foreign import slice "function slice(s) {\
                       \  return function(e) {\
                       \    return function (l) {\
                       \      return l.slice(s, e);\
                       \    };\
                       \  };\
                       \}" :: forall a. Number -> Number -> [a] -> [a]

  foreign import sort "function sort(l) {\
                      \  var l1 = l.slice();\
                      \  l1.sort();\
                      \  return l1;\
                      \}" :: forall a. [a] -> [a]

  foreign import insertAt
    "function insertAt(index) {\
    \  return function(a) {\
    \    return function(l) {\
    \      var l1 = l.slice();\
    \      l1.splice(index, 0, a);\
    \      return l1;\
    \    }; \
    \  };\
    \}":: forall a. Number -> a -> [a] -> [a]

  foreign import deleteAt
    "function deleteAt(index) {\
    \  return function(n) {\
    \    return function(l) {\
    \      var l1 = l.slice();\
    \      l1.splice(index, n);\
    \      return l1;\
    \    }; \
    \  };\
    \}":: forall a. Number -> Number -> [a] -> [a]

  foreign import updateAt
    "function updateAt(index) {\
    \  return function(a) {\
    \    return function(l) {\
    \      var l1 = l.slice();\
    \      l1[index] = a;\
    \      return l1;\
    \    }; \
    \  };\
    \}":: forall a. Number -> a -> [a] -> [a]

  infixr 6 :

  (:) :: forall a. a -> [a] -> [a]
  (:) a = concat [a]

  singleton :: forall a. a -> [a]
  singleton a = [a]

  concatMap :: forall a b. [a] -> (a -> [b]) -> [b]
  concatMap [] f = []
  concatMap (a:as) f = f a `concat` concatMap as f

  filter :: forall a. (a -> Boolean) -> [a] -> [a]
  filter _ [] = []
  filter p (x:xs) | p x = x : filter p xs
  filter p (_:xs) = filter p xs

  isEmpty :: forall a. [a] -> Boolean
  isEmpty [] = true
  isEmpty _ = false

  range :: Number -> Number -> [Number]
  range lo hi | lo > hi = []
  range lo hi = lo : range (lo + 1) hi

  zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
  zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
  zipWith _ _ _ = []

  drop :: forall a. Number -> [a] -> [a]
  drop 0 xs = xs
  drop _ [] = []
  drop n (x:xs) = drop (n - 1) xs

  take :: forall a. Number -> [a] -> [a]
  take 0 _ = []
  take _ [] = []
  take n (x:xs) = x : take (n - 1) xs

  instance showArray :: (Show a) => Show [a] where
    show xs = "[" ++ joinWith (map show xs) "," ++ "]"

  instance monadArray :: Monad [] where
    return = singleton
    (>>=) = concatMap

  instance functorArray :: Functor [] where
    (<$>) = map

  instance alternativeArray :: Alternative [] where
    empty = []
    (<|>) = concat

module Data.Eq where

  import Prelude

  -- Referential equality
  data Ref a = Ref a

  liftRef :: forall a b. (a -> a -> b) -> Ref a -> Ref a -> b
  liftRef f (Ref x) (Ref y) = f x y

  refEq :: forall a. Ref a -> Ref a -> Boolean
  refEq = liftRef unsafeRefEq

  refIneq :: forall a. Ref a -> Ref a -> Boolean
  refIneq = liftRef unsafeRefIneq

  instance eqRef :: Eq (Ref a) where
    (==) = refEq
    (/=) = refIneq

module Data.Array.Unsafe where

  head :: forall a. [a] -> a
  head (x : _) = x

  tail :: forall a. [a] -> [a]
  tail (_ : xs) = xs

module Data.Tuple where

  import Prelude
  import Data.Array

  data Tuple a b = Tuple a b

  instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
    show (Tuple a b) = "Tuple(" ++ show a ++ ", " ++ show b ++ ")"

  curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c
  curry f a b = f (Tuple a b)

  uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c
  uncurry f (Tuple a b) = f a b

  zip :: forall a b. [a] -> [b] -> [Tuple a b]
  zip = zipWith Tuple

  unzip :: forall a b. [Tuple a b] -> Tuple [a] [b]
  unzip ((Tuple a b):ts) = case unzip ts of
    Tuple as bs -> Tuple (a : as) (b : bs)
  unzip [] = Tuple [] []

  instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a1 b1) (Tuple a2 b2) = a1 == a2 && b1 == b2
    (/=) t1 t2 = not (t1 == t2)

  instance functorTuple :: Functor (Tuple a) where
    (<$>) f (Tuple x y) = Tuple x (f y)

module Data.String where

  foreign import lengthS "function lengthS(s) {\
                         \  return s.length;\
                         \}" :: String -> Number

  foreign import charAt "function charAt(i) {\
                        \  return function(s) {\
                        \    return s.charAt(i); \
                        \  };\
                        \}" :: Number -> String -> String

  foreign import indexOfS "function indexOfS(s1) {\
                          \  return function(s2) {\
                          \    return s1.indexOf(s2);\
                          \  }; \
                          \}" :: String -> String -> Number

  foreign import lastIndexOfS "function lastIndexOfS(s1) {\
                              \  return function(s2) {\
                              \    return s1.lastIndexOf(s2);\
                              \  };\
                              \}" :: String -> String -> Number

  foreign import localeCompare "function localeCompare(s1) {\
                               \  return function(s2) {\
                               \    return s1.localeCompare(s2);\
                               \  };\
                               \}" :: String -> String -> Number

  foreign import replace "function replace(s1) {\
                         \  return function(s2) {\
                         \    return function(s3) {\
                         \      return s3.replace(s1, s2);\
                         \    };\
                         \  };\
                         \}" :: String -> String -> String -> String

  foreign import sliceS "function sliceS(st) {\
                        \  return function(e) {\
                        \    return function(s) {\
                        \      return s.slice(st, e);\
                        \    };\
                        \  };\
                        \}" :: Number -> Number -> String -> String

  foreign import split "function split(sep) {\
                       \  return function(s) {\
                       \    return s.split(sep);\
                       \  };\
                       \}" :: String -> String -> [String]

  foreign import substr "function substr(n1) {\
                        \  return function(n2) {\
                        \    return function(s) {\
                        \      return s.substr(n1, n2);\
                        \    };\
                        \  };\
                        \}" :: Number -> Number -> String -> String

  foreign import substring "function substring(n1) {\
                           \  return function(n2) {\
                           \    return function(s) {\
                           \      return s.substring(n1, n2);\
                           \    };\
                           \  };\
                           \}" :: Number -> Number -> String -> String

  foreign import toLower "function toLower(s) {\
                         \  return s.toLowerCase();\
                         \}" :: String -> String

  foreign import toUpper "function toUpper(s) {\
                         \  return s.toUpperCase();\
                         \}" :: String -> String

  foreign import trim "function trim(s) {\
                      \  return s.trim();\
                      \}" :: String -> String

module Data.String.Regex where

  foreign import data Regex :: *

  foreign import regex "function regex(s1) {\
                       \  return function(s2) {\
                       \    return new Regex(s1, s2);\
                       \  };\
                       \}" :: String -> String -> Regex

  foreign import test "function test(r) {\
                      \  return function (s) {\
                      \    return r.test(s);\
                      \  };\
                      \}" :: Regex -> String -> Boolean

  foreign import match "function match(r) {\
                       \  return function (s) {\
                       \    return s.match(r); \
                       \  };\
                       \}" :: Regex -> String -> [String]

  foreign import replaceR "function replaceR(r) {\
                          \  return function(s1) {\
                          \    return function(s2) {\
                          \      return s2.replace(r, s1);\
                          \    };\
                          \  };\
                          \}" :: Regex -> String -> String -> String

  foreign import search "function search(r) {\
                        \  return function (s) {\
                        \    return s.search(r);\
                        \  };\
                        \}" :: Regex -> String -> Number

module Global where

  foreign import nan "var nan = NaN;" :: Number

  foreign import infinity "var infinity = Infinity;" :: Number

  foreign import toExponential "function toExponential(n) {\
                               \  return n.toExponential();\
                               \}" :: Number -> String

  foreign import toFixed "function toFixed(d) {\
                         \  return function(n) {\
                         \    return n.toFixed(d);\
                         \  };\
                         \}" :: Number -> Number -> String

  foreign import toPrecision "function toPrecision(d) {\
                             \  return function(n) {\
                             \    return n.toPrecision(d);\
                             \  };\
                             \}" :: Number -> Number -> String

  foreign import isFinite :: Number -> Boolean

  foreign import parseFloat :: String -> Number

  foreign import parseInt :: String -> Number

  foreign import encodeURIComponent :: String -> String

  foreign import decodeURIComponent :: String -> String

  foreign import encodeURI :: String -> String

  foreign import decodeURI :: String -> String

  foreign import isNaN :: Number -> Boolean

module Math where

  foreign import abs "function abs(n){\
                     \  return Math.abs(n);\
                     \}" :: Number -> Number

  foreign import acos "function acos(n){\
                      \  return Math.acos(n);\
                      \}" :: Number -> Number

  foreign import asin "function asin(n){\
                      \  return Math.asin(n);\
                      \}" :: Number -> Number

  foreign import atan "function atan(n){\
                      \  return Math.atan(n);\
                      \}" :: Number -> Number

  foreign import atan2 "function atan2(y){\
                       \  return function (x) {\
                       \    return Math.atan2(y, x);\
                       \  };\
                       \}" :: Number -> Number -> Number

  foreign import aceil "function aceil(n){\
                       \  return Math.aceil(n);\
                       \}" :: Number -> Number

  foreign import cos "function cos(n){\
                     \  return Math.cos(n);\
                     \}" :: Number -> Number

  foreign import exp "function exp(n){\
                     \  return Math.exp(n);\
                     \}" :: Number -> Number

  foreign import floor "function floor(n){\
                       \  return Math.floor(n);\
                       \}" :: Number -> Number

  foreign import log "function log(n){\
                     \  return Math.log(n);\
                     \}" :: Number -> Number

  foreign import max "function max(n1){\
                     \  return function(n2) {\
                     \    return Math.max(n1, n2);\
                     \  }\
                     \}" :: Number -> Number -> Number

  foreign import min "function min(n1){\
                     \  return function(n2) {\
                     \    return Math.min(n1, n2);\
                     \  }\
                     \}" :: Number -> Number -> Number

  foreign import pow "function pow(n){\
                     \  return function(p) {\
                     \    return Math.pow(n, p);\
                     \  }\
                     \}" :: Number -> Number -> Number

  foreign import round "function round(n){\
                       \  return Math.round(n);\
                       \}" :: Number -> Number

  foreign import sin "function sin(n){\
                     \  return Math.sin(n);\
                     \}" :: Number -> Number

  foreign import sqrt "function sqrt(n){\
                      \  return Math.sqrt(n);\
                      \}" :: Number -> Number

  foreign import tan "function tan(n){\
                     \  return Math.tan(n);\
                     \}" :: Number -> Number

  foreign import e       "var e       = Math.E;"       :: Number
  foreign import ln2     "var ln2     = Math.LN2;"     :: Number
  foreign import ln10    "var ln10    = Math.LN10;"    :: Number
  foreign import log2e   "var log2e   = Math.LOG2E;"   :: Number
  foreign import log10e  "var log10e  = Math.LOG10E;"  :: Number
  foreign import pi      "var pi      = Math.PI;"      :: Number
  foreign import sqrt1_2 "var sqrt1_2 = Math.SQRT1_2;" :: Number
  foreign import sqrt2   "var sqrt2   = Math.SQRT2;"   :: Number

module Control.Monad.Eff where

  import Prelude

  foreign import data Eff :: # ! -> * -> *

  foreign import retEff "function retEff(a) {\
                        \  return function() {\
                        \    return a;\
                        \  };\
                        \}" :: forall e a. a -> Eff e a

  foreign import bindEff "function bindEff(a) {\
                         \  return function(f) {\
                         \    return function() {\
                         \      return f(a())();\
                         \    };\
                         \  };\
                         \}" :: forall e a b. Eff e a -> (a -> Eff e b) -> Eff e b

  type Pure a = forall e. Eff e a

  foreign import runPure "function runPure(f) {\
                         \  return f();\
                         \}" :: forall a. Pure a -> a

  instance monadEff :: Monad (Eff e) where
    return = retEff
    (>>=) = bindEff

  foreign import untilE "function untilE(f) {\
                        \  return function() {\
                        \    while (!f()) { }\
                        \    return {};\
                        \  };\
                        \}" :: forall e. Eff e Boolean -> Eff e {}

  foreign import whileE "function whileE(f) {\
                        \  return function(a) {\
                        \    return function() {\
                        \      while (f()) {\
                        \        a();\
                        \      }\
                        \      return {};\
                        \    };\
                        \  };\
                        \}" :: forall e a. Eff e Boolean -> Eff e a -> Eff e {}

  foreign import forE "function forE(lo) {\
                      \  return function(hi) {\
                      \    return function(f) {\
                      \      return function() {\
                      \        for (var i = lo; i < hi; i++) {\
                      \          f(i)();\
                      \        }\
                      \      };\
                      \    };\
                      \  };\
                      \}" :: forall e. Number -> Number -> (Number -> Eff e {}) -> Eff e {}


  foreign import foreachE "function foreachE(as) {\
                          \  return function(f) {\
                          \    for (var i = 0; i < as.length; i++) {\
                          \      f(as[i])();\
                          \    }\
                          \  };\
                          \}" :: forall e a. [a] -> (a -> Eff e {}) -> Eff e {}

module Control.Monad.Eff.Unsafe where

  import Control.Monad.Eff

  foreign import unsafeInterleaveEff
    "function unsafeInterleaveEff(f) {\
    \  return f;\
    \}" :: forall eff1 eff2 a. Eff eff1 a -> Eff eff2 a

module Random where

  import Control.Monad.Eff

  foreign import data Random :: !

  foreign import random "function random() {\
                        \  return Math.random();\
                        \}" :: forall e. Eff (random :: Random | e) Number

module Control.Monad.Eff.Error where

  import Control.Monad.Eff

  foreign import data Error :: * -> !

  foreign import throwError "function throwError(e) {\
                            \  return function() {\
                            \    throw e;\
                            \  };\
                            \}" :: forall a e r. e -> Eff (err :: Error e | r) a

  foreign import catchError "function catchError(c) {\
                            \  return function(t) {\
                            \    return function() {\
                            \      try {\
                            \        return t();\
                            \      } catch(e) {\
                            \        return c(e)();\
                            \      }\
                            \    };\
                            \  };\
                            \}" :: forall e r a. (e -> Eff r a) -> Eff (err :: Error e | r) a -> Eff r a

module Data.IORef where

  import Control.Monad.Eff

  foreign import data Ref :: !

  foreign import data IORef :: * -> *

  foreign import newIORef "function newIORef(val) {\
                          \  return function () {\
                          \    return { value: val };\
                          \  };\
                          \}" :: forall s r. s -> Eff (ref :: Ref | r) (IORef s)

  foreign import readIORef "function readIORef(ref) {\
                           \  return function() {\
                           \    return ref.value;\
                           \  };\
                           \}" :: forall s r. IORef s -> Eff (ref :: Ref | r) s


  foreign import modifyIORef "function modifyIORef(ref) {\
                             \  return function(f) {\
                             \    return function() {\
                             \      ref.value = f(ref.value);\
                             \      return {};\
                             \    };\
                             \  };\
                             \}" :: forall s r. IORef s -> (s -> s) -> Eff (ref :: Ref | r) {}

  foreign import writeIORef "function writeIORef(ref) {\
                            \  return function(val) {\
                            \    return function() {\
                            \      ref.value = val;\
                            \      return {};\
                            \    };\
                            \  };\
                            \}" :: forall s r. IORef s -> s -> Eff (ref :: Ref | r) {}

  foreign import unsafeRunIORef "function unsafeRunIORef(f) {\
                                \  return f;\
                                \}" :: forall eff a. Eff (ref :: Ref | eff) a -> Eff eff a

module Debug.Trace where

  import Prelude
  import Control.Monad.Eff

  foreign import data Trace :: !

  foreign import trace "function trace(s) {\
                       \  return function() {\
                       \    console.log(s);\
                       \    return {};\
                       \  };\
                       \}" :: forall r. String -> Eff (trace :: Trace | r) {}

  print :: forall a r. (Show a) => a -> Eff (trace :: Trace | r) {}
  print o = trace (show o)

module Control.Monad.ST where

  import Control.Monad.Eff

  foreign import data ST :: * -> !

  foreign import data STRef :: * -> * -> *

  foreign import data STArray :: * -> * -> *

  foreign import newSTRef "function newSTRef(val) {\
                          \  return function () {\
                          \    return { value: val };\
                          \  };\
                          \}" :: forall a h r. a -> Eff (st :: ST h | r) (STRef h a)

  foreign import readSTRef "function readSTRef(ref) {\
                           \  return function() {\
                           \    return ref.value;\
                           \  };\
                           \}" :: forall a h r. STRef h a -> Eff (st :: ST h | r) a

  foreign import modifySTRef "function modifySTRef(ref) {\
                             \  return function(f) {\
                             \    return function() {\
                             \      return ref.value = f(ref.value);\
                             \    };\
                             \  };\
                             \}" :: forall a h r. STRef h a -> (a -> a) -> Eff (st :: ST h | r) a

  foreign import writeSTRef "function writeSTRef(ref) {\
                            \  return function(a) {\
                            \    return function() {\
                            \      return ref.value = a;\
                            \    };\
                            \  };\
                            \}" :: forall a h r. STRef h a -> a -> Eff (st :: ST h | r) a

  foreign import newSTArray "function newSTArray(len) {\
                            \  return function(a) {\
                            \    return function() {\
                            \      var arr = [];\
                            \      for (var i = 0; i < len; i++) {\
                            \        arr[i] = a;\
                            \      };\
                            \      return arr;\
                            \    };\
                            \  };\
                            \}" :: forall a h r. Number -> a -> Eff (st :: ST h | r) (STArray h a)

  foreign import peekSTArray "function peekSTArray(arr) {\
                             \  return function(i) {\
                             \    return function() {\
                             \      return arr[i];\
                             \    };\
                             \  };\
                             \}" :: forall a h r. STArray h a -> Eff (st :: ST h | r) a

  foreign import pokeSTArray "function pokeSTArray(arr) {\
                             \  return function(i) {\
                             \    return function(a) {\
                             \      return function() {\
                             \        return arr[i] = a;\
                             \      };\
                             \    };\
                             \  };\
                             \}" :: forall a h r. STArray h a -> Number -> a -> Eff (st :: ST h | r) a

  foreign import runST "function runST(f) {\
                       \  return f;\
                       \}" :: forall a r. (forall h. Eff (st :: ST h | r) a) -> Eff r a

  foreign import runSTArray "function runSTArray(f) {\
                            \  return f;\
                            \}" :: forall a r. (forall h. Eff (st :: ST h | r) (STArray h a)) -> Eff r [a]

module Data.Enum where

  import Data.Maybe

  class Enum a where
    toEnum :: Number -> Maybe a
    fromEnum :: a -> Number

module Text.Parsing.Read where

  class Read a where
    read :: String -> a

  instance readString :: Read String where
    read s = s

  instance readBoolean :: Read Boolean where
    read "true" = true
    read _ = false

  foreign import readNumberImpl "var readNumberImpl = parseFloat;" :: String -> Number

  instance readNumber :: Read Number where
    read = readNumberImpl

module Data.Foldable where

  import Prelude
  import Control.Applicative
  import Data.Either
  import Data.Eq
  import Data.Maybe
  import Data.Monoid
  import Data.Tuple

  class Foldable f where
    foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
    foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
    foldMap :: forall a m. (Monoid m) => (a -> m) -> f a -> m

  instance foldableArray :: Foldable [] where
    foldr _ z []     = z
    foldr f z (x:xs) = x `f` (foldr f z xs)

    foldl _ z []     = z
    foldl f z (x:xs) = foldl f (z `f` x) xs

    foldMap _ []     = mempty
    foldMap f (x:xs) = f x <> foldMap f xs

  instance foldableEither :: Foldable (Either a) where
    foldr _ z (Left _)  = z
    foldr f z (Right x) = x `f` z

    foldl _ z (Left _)  = z
    foldl f z (Right x) = z `f` x

    foldMap f (Left _)  = mempty
    foldMap f (Right x) = f x

  instance foldableMaybe :: Foldable Maybe where
    foldr _ z Nothing  = z
    foldr f z (Just x) = x `f` z

    foldl _ z Nothing  = z
    foldl f z (Just x) = z `f` x

    foldMap f Nothing  = mempty
    foldMap f (Just x) = f x

  instance foldableRef :: Foldable Ref where
    foldr f z (Ref x) = x `f` z

    foldl f z (Ref x) = z `f` x

    foldMap f (Ref x) = f x

  instance foldableTuple :: Foldable (Tuple a) where
    foldr f z (Tuple _ x) = x `f` z

    foldl f z (Tuple _ x) = z `f` x

    foldMap f (Tuple _ x) = f x

  fold :: forall f m. (Foldable f, Monoid m) => f m -> m
  fold = foldMap id

  traverse_ :: forall a b f m. (Applicative m, Foldable f) => (a -> m b) -> f a -> m {}
  traverse_ f = foldr ((*>) <<< f) (pure {})

  for_ :: forall a b f m. (Applicative m, Foldable f) => f a -> (a -> m b) -> m {}
  for_ = flip traverse_

  sequence_ :: forall a f m. (Applicative m, Foldable f) => f (m a) -> m {}
  sequence_ = traverse_ id

  mconcat :: forall f m. (Foldable f, Monoid m) => f m -> m
  mconcat = foldl (<>) mempty

  and :: forall f. (Foldable f) => f Boolean -> Boolean
  and = foldl (&&) true

  or :: forall f. (Foldable f) => f Boolean -> Boolean
  or = foldl (||) false

  any :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Boolean
  any p = or <<< foldMap (\x -> [p x])

  all :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Boolean
  all p = and <<< foldMap (\x -> [p x])

  sum :: forall f. (Foldable f) => f Number -> Number
  sum = foldl (+) 0

  product :: forall f. (Foldable f) => f Number -> Number
  product = foldl (*) 1

  elem :: forall a f. (Eq a, Foldable f) => a -> f a -> Boolean
  elem = any  <<< (==)

  notElem :: forall a f. (Eq a, Foldable f) => a -> f a -> Boolean
  notElem x = not <<< elem x

  find :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Maybe a
  find p f = case foldMap (\x -> if p x then [x] else []) f of
    (x:_) -> Just x
    []    -> Nothing

module Data.Traversable where

  import Prelude
  import Data.Array ((:))
  import Data.Either
  import Data.Eq
  import Data.Foldable
  import Data.Maybe
  import Data.Tuple

  class Traversable t where
    traverse :: forall a b m. (Applicative m) => (a -> m b) -> t a -> m (t b)
    sequence :: forall a m. (Applicative m) => t (m a) -> m (t a)

  instance traversableArray :: Traversable [] where
    traverse _ []     = pure []
    traverse f (x:xs) = (:) <$> (f x) <*> traverse f xs

    sequence []     = pure []
    sequence (x:xs) = (:) <$> x <*> sequence xs

  instance traversableEither :: Traversable (Either a) where
    traverse _ (Left x)  = pure (Left x)
    traverse f (Right x) = Right <$> f x

    sequence (Left x) = pure (Left x)
    sequence (Right x)  = Right <$> x

  instance traversableRef :: Traversable Ref where
    traverse f (Ref x) = Ref <$> f x

    sequence (Ref x) = Ref <$> x

  instance traversableMaybe :: Traversable Maybe where
    traverse _ Nothing  = pure Nothing
    traverse f (Just x) = Just <$> f x

    sequence Nothing  = pure Nothing
    sequence (Just x) = Just <$> x

  instance traversableTuple :: Traversable (Tuple a) where
    traverse f (Tuple x y) = Tuple x <$> f y

    sequence (Tuple x y) = Tuple x <$> y

  for :: forall a b m t. (Applicative m, Traversable t) => t a -> (a -> m b) -> m (t b)
  for x f = traverse f x

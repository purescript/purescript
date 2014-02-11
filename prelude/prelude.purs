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
    (>>>) :: forall b c d. a b c -> a c d -> a b d

  instance Category (->) where
    id x = x
    (<<<) f g x = f (g x)
    (>>>) f g x = g (f x)

  infixr 0 $
  infixl 0 #

  ($) :: forall a b. (a -> b) -> a -> b
  ($) f x = f x

  (#) :: forall a b. a -> (a -> b) -> b
  (#) x f = f x

  class Show a where
    show :: a -> String

  instance Show String where
    show s = s

  instance Show Boolean where
    show true = "true"
    show false = "false"

  foreign import showNumber "function showNumber(n) {\
                            \  return n.toString();\
                            \}" :: Number -> String

  instance Prelude.Show Number where
    show = showNumber

  class Read a where
    read :: String -> a

  instance Read String where
    read s = s

  instance Read Boolean where
    read "true" = true
    read _ = false
    
  foreign import readNumber "function readNumber(n) {\
                            \  return parseInt(n, 10);\
                            \}" :: String -> Number
    
  instance Read Number where
    read = readNumber

  infixl 4 <$>

  class Functor f where
    (<$>) :: forall a b. (a -> b) -> f a -> f b

  infixl 4 <*>

  class Applicative f where
    pure :: forall a. a -> f a
    (<*>) :: forall a b. f (a -> b) -> f a -> f b

  instance (Applicative f) => Functor f where
    (<$>) f a = pure f <*> a

  infixl 3 <|>

  class Alternative f where
    empty :: forall a. f a
    (<|>) :: forall a. f a -> f a -> f a

  infixl 1 >>=

  class Monad m where
    return :: forall a. a -> m a
    (>>=) :: forall a b. m a -> (a -> m b) -> m b

  instance (Monad m) => Applicative m where
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

  instance Num Number where
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

  -- Referential equality
  data Ref a = Ref a

  foreign import refEq "function refEq(r1) {\
                       \  return function(r2) {\
                       \    return r1.value === r2.value;\
                       \  };\
                       \}" :: forall a. Ref a -> Ref a -> Boolean

  foreign import refIneq "function refIneq(r1) {\
                         \  return function(r2) {\
                         \    return r1.value !== r2.value;\
                         \  };\
                         \}" :: forall a. Ref a -> Ref a -> Boolean

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

  instance Eq (Ref a) where
    (==) = refEq
    (/=) = refIneq

  instance Eq String where
    (==) = unsafeRefEq
    (/=) = unsafeRefIneq

  instance Eq Number where
    (==) = unsafeRefEq
    (/=) = unsafeRefIneq

  instance Eq Boolean where
    (==) = unsafeRefEq
    (/=) = unsafeRefIneq

  instance (Eq a) => Eq [a] where
    (==) [] [] = true
    (==) (x:xs) (y:ys) = x == y && xs == ys
    (==) _ _ = false
    (/=) xs ys = not (xs == ys)

  infixl 4 <
  infixl 4 >
  infixl 4 <=
  infixl 4 >=

  class Ord a where
    (<) :: a -> a -> Boolean
    (>) :: a -> a -> Boolean
    (<=) :: a -> a -> Boolean
    (>=) :: a -> a -> Boolean

  foreign import numLess "function numLess(n1) {\
                         \  return function(n2) {\
                         \    return n1 < n2;\
                         \  };\
                         \}" :: Number -> Number -> Boolean

  foreign import numLessEq "function numLessEq(n1) {\
                           \  return function(n2) {\
                           \    return n1 <= n2;\
                           \  };\
                           \}" :: Number -> Number -> Boolean

  foreign import numGreater "function numGreater(n1) {\
                            \  return function(n2) {\
                            \    return n1 > n2;\
                            \  };\
                            \}" :: Number -> Number -> Boolean

  foreign import numGreaterEq "function numGreaterEq(n1) {\
                              \  return function(n2) {\
                              \    return n1 >= n2;\
                              \  };\
                              \}" :: Number -> Number -> Boolean

  instance Ord Number where
    (<) = numLess
    (>) = numGreater
    (<=) = numLessEq
    (>=) = numGreaterEq

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

  instance Bits Number where
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

  instance BoolLike Boolean where
    (&&) = boolAnd
    (||) = boolOr
    not = boolNot

  infixr 5 ++

  foreign import (++) "function $plus$plus(s1) {\
                      \  return function(s2) {\
                      \    return s1 + s2;\
                      \  };\
                      \}" :: String -> String -> String

module Monoid where

  import Prelude

  infixr 6 <>

  class Monoid m where
    mempty :: m
    (<>) :: m -> m -> m

  instance Monoid String where
    mempty = ""
    (<>) = (++)
    
  instance Monoid [a] where
    mempty = []
    (<>) = Arrays.concat

  mconcat :: forall m. (Monoid m) => [m] -> m
  mconcat [] = mempty
  mconcat (m:ms) = m <> mconcat ms

module Monad where

  import Prelude
  import Arrays

  replicateM :: forall m a. (Monad m) => Number -> m a -> m [a]
  replicateM 0 _ = return []
  replicateM n m = do
    a <- m
    as <- replicateM (n - 1) m
    return (a : as)

  mapM :: forall m a b. (Monad m) => (a -> m b) -> [a] -> m [b]
  mapM _ [] = return []
  mapM f (a:as) = do
    b <- f a
    bs <- mapM f as
    return (b : bs)

  infixr 1 >=>
  infixr 1 <=<

  (>=>) :: forall m a b c. (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
  (>=>) f g a = do
    b <- f a
    g b

  (<=<) :: forall m a b c. (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
  (<=<) = flip (>=>)

  sequence :: forall m a. (Monad m) => [m a] -> m [a]
  sequence [] = return []
  sequence (m:ms) = do
    a <- m
    as <- sequence ms
    return (a : as)

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

module Maybe where

  import Prelude

  data Maybe a = Nothing | Just a

  maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
  maybe b _ Nothing = b
  maybe _ f (Just a) = f a

  fromMaybe :: forall a. a -> Maybe a -> a
  fromMaybe a = maybe a (Prelude.id :: forall a. a -> a)
  
  instance Prelude.Monad Maybe where
    return = Just
    (>>=) m f = maybe Nothing f m
    
  instance Prelude.Applicative Maybe where
    pure = Just
    (<*>) (Just fn) x = fn <$> x
    (<*>) Nothing _ = Nothing

  instance Prelude.Functor Maybe where
    (<$>) fn (Just x) = Just (fn x)
    (<$>) _ _ = Nothing
  
  instance (Show a) => Prelude.Show (Maybe a) where
    show (Just x) = "Just " ++ (show x)
    show Nothing = "Nothing"

module Either where

  import Prelude

  data Either a b = Left a | Right b

  either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
  either f _ (Left a) = f a
  either _ g (Right b) = g b
  
  instance Prelude.Monad (Either e) where
    return = Right
    (>>=) = either (\e _ -> Left e) (\a f -> f a)
    
  instance Prelude.Applicative (Either e) where
    pure = Right
    (<*>) (Left e) _ = Left e
    (<*>) (Right f) r = f <$> r

  instance Prelude.Functor (Either a) where
    (<$>) _ (Left x) = Left x
    (<$>) f (Right y) = Right (f y)
    
  instance (Show a, Show b) => Prelude.Show (Either a b) where
    show (Left x) = "Left " ++ (show x)
    show (Right y) = "Right " ++ (show y)

module Arrays where

  import Prelude
  import Maybe

  head :: forall a. [a] -> a
  head (x : _) = x

  headSafe :: forall a. [a] -> Maybe a
  headSafe (x : _) = Just x
  headSafe _ = Nothing

  tail :: forall a. [a] -> [a]
  tail (_ : xs) = xs

  tailSafe :: forall a. [a] -> Maybe [a]
  tailSafe (_ : xs) = Just xs
  tailSafe _ = Nothing

  map :: forall a b. (a -> b) -> [a] -> [b]
  map _ [] = []
  map f (x:xs) = f x : map f xs

  foldr :: forall a b. (a -> b -> a) -> a -> [b] -> a
  foldr f a (b : bs) = f (foldr f a bs) b
  foldr _ a [] = a

  foldl :: forall a b. (a -> b -> b) -> b -> [a] -> b
  foldl _ b [] = b
  foldl f b (a:as) = foldl f (f a b) as

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
                      \  l.sort();\
                      \  return l1;\
                      \}" :: forall a. [a] -> [a]

  foreign import splice "function splice(s) {\
                        \  return function(e) {\
                        \    return function(l1) {\
                        \      return function(l2) {\
                        \        return l2.splice(s, e, l1);\
                        \      }; \
                        \    }; \
                        \  };\
                        \}":: forall a. Number -> Number -> [a] -> [a] -> [a]

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

  any :: forall a. (a -> Boolean) -> [a] -> Boolean
  any _ [] = false
  any p (a:as) = p a || any p as

  all :: forall a. (a -> Boolean) -> [a] -> Boolean
  all _ [] = true
  all p (a:as) = p a && all p as

  instance (Prelude.Show a) => Prelude.Show [a] where
    show [] = "[]"
    show (x:xs) = show x ++ " : " ++ show xs
    
  instance Prelude.Functor [] where
    (<$>) = map
    
  instance Prelude.Monad [] where
    return = singleton
    (>>=) = concatMap

  instance Prelude.Alternative [] where
    empty = []
    (<|>) = concat

module Tuples where

  import Prelude
  import Arrays

  data Tuple a b = Tuple a b

  instance (Prelude.Show a, Prelude.Show b) => Prelude.Show (Tuple a b) where
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

module String where

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
                       \    return s.split(s);\
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
                         \  return s.toLower();\
                         \}" :: String -> String

  foreign import toUpper "function toUpper(s) {\
                         \  return s.toUpper();\
                         \}" :: String -> String

  foreign import trim "function trim(s) {\
                      \  return s.trim();\
                      \}" :: String -> String

module Regex where

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

  foreign import max "function max(n){\
                     \  return Math.max(n);\
                     \}" :: Number -> Number

  foreign import min "function min(n){\
                     \  return Math.min(n);\
                     \}" :: Number -> Number

  foreign import pow "function pow(n){\
                     \  return Math.pow(n);\
                     \}" :: Number -> Number

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

module Eff where

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

  instance Prelude.Monad (Eff e) where
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
                        \}" :: forall e. Eff e Boolean -> Eff e {} -> Eff e {}

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


module Random where

  import Eff

  foreign import data Random :: !

  foreign import random "function random() {\
                        \  return Math.random();\
                        \}" :: forall e. Eff (random :: Random | e) Number

module Errors where

  import Eff

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

module IORef where

  import Eff

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

module Trace where

  import Prelude
  import Eff

  foreign import data Trace :: !

  foreign import trace "function trace(s) {\
                       \  return function() {\
                       \    console.log(s);\
                       \    return {};\
                       \  };\
                       \}" :: forall r. String -> Eff (trace :: Trace | r) {}

  print :: forall a r. (Prelude.Show a) => a -> Eff (trace :: Trace | r) {}
  print o = trace (show o)

module ST where

  import Eff

  foreign import data ST :: * -> !

  foreign import data STRef :: * -> * -> *

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
                             \      ref.value = f(ref.value);\
                             \    };\
                             \  };\
                             \}" :: forall a h r. STRef h a -> (a -> a) -> Eff (st :: ST h | r) {}

  foreign import writeSTRef "function writeSTRef(ref) {\
                            \  return function(a) {\
                            \    return function() {\
                            \      ref.value = a;\
                            \    };\
                            \  };\
                            \}" :: forall a h r. STRef h a -> a -> Eff (st :: ST h | r) {}

  foreign import runST "function runST(f) {\
                       \  return f;\
                       \}" :: forall a r. (forall h. Eff (st :: ST h | r) a) -> Eff r a


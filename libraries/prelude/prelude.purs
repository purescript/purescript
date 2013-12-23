id :: forall a. a -> a
id = \x -> x

flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip = \f -> \b -> \a -> f a b

konst :: forall a b. a -> b -> a
konst = \a -> \b -> a

(|>) :: forall a b c. (a -> b) -> (b -> c) -> a -> c
(|>) = \f -> \g -> \a -> g (f a)

(<|) :: forall a b c. (b -> c) -> (a -> b) -> a -> c
(<|) = flip (|>)

-- Maybe

data Maybe a = Nothing | Just a

maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe = \b -> \f -> \m -> case m of 
  Nothing -> b
  Just a -> f a

fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe = \a -> maybe a id

bindMaybe :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe = \m -> \f -> maybe Nothing f m

-- Either

data Either a b = Left a | Right b

either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
either = \f -> \g -> \e -> case e of
  Left a -> f a
  Right b -> g b 

bindEither :: forall e a b. Either e a -> (a -> Either e b) -> Either e b
bindEither = either (\e -> \_ -> Left e) (\a -> \f -> f a) 

-- Arrays

head :: forall a. [a] -> a
head = \xs -> case xs of
  x : _ -> x

headSafe :: forall a. [a] -> Maybe a
headSafe = \xs -> case xs of 
  x : _ -> Just x
  _ -> Nothing

tail :: forall a. [a] -> [a]
tail = \xs -> case xs of
  _ : xs -> xs

tailSafe :: forall a. [a] -> Maybe [a]
tailSafe = \xs -> case xs of
  _ : xs -> Just xs
  _ -> Nothing

foreign import map :: forall a b. (a -> b) -> [a] -> [b]

foldr :: forall a b. (a -> b -> a) -> a -> [b] -> a
foldr = \f -> \a -> \bs -> case bs of
  b : bs -> f (foldr f a bs) b
  [] -> a

foldl :: forall a b. (a -> b -> b) -> b -> [a] -> b
foldl = \f -> \b -> \as -> {
    var result = b;
    foreach (a in as) {
      result = f a result;
    }
    return result;
  }

foreign import member "length" length :: forall a. [a] -> Number

foreign import indexOf :: forall a. [a] -> a -> Number

foreign import lastIndexOf :: forall a. [a] -> a -> Number

foreign import concat :: forall a. [a] -> [a] -> [a]

foreign import join :: [String] -> String

foreign import joinWith :: [String] -> String -> String

foreign import push :: forall a. [a] -> a -> [a]

foreign import reverse :: forall a. [a] -> [a]

foreign import shift :: forall a. [a] -> [a]

foreign import slice :: forall a. Number -> Number -> [a] -> [a]

foreign import sort :: forall a. [a] -> [a]

foreign import splice :: forall a. Number -> Number -> [a] -> [a] -> [a]

cons :: forall a. a -> [a] -> [a]
cons = \a -> concat([a])

concatMap :: forall a b. [a] -> (a -> [b]) -> [b]
concatMap = \as -> \f -> {
    var result = [];
    foreach (a in as) {
      result = result `concat` f a;
    }
    return result;
  }

foreign import filter :: forall a. [a] -> (a -> Boolean) -> [a]

empty :: forall a. [a] -> Boolean
empty = \as -> case as of
  [] -> true
  _ -> false

range :: Number -> Number -> [Number]
range = \lo -> \hi -> {
    var ns = [];
    for (n <- lo until hi) {
      ns = push ns n;
    }
    return ns;
  }

zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = \f -> \as -> \bs -> case { as: as, bs: bs } of
  { as = a : as1, bs = b : bs1 } -> cons (f a b) (zipWith f as1 bs1)
  _ -> []

any :: forall a. (a -> Boolean) -> [a] -> Boolean
any = \p -> \as -> {
    for (i <- 0 until length as) {
      if (p (as !! i)) {
        return true;
      }
    }
    return false;
  }

all :: forall a. (a -> Boolean) -> [a] -> Boolean
all = \p -> \as -> {
    for (i <- 0 until length as) {
      if (!(p (as !! i))) {
        return false;
      }
    }
    return true;
  }

-- Pairs

type Tuple a b = { fst :: a, snd :: b }

curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c
curry = \f -> \a -> \b -> f { fst: a, snd: b }

uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c
uncurry = \f -> \t -> f t.fst t.snd

tuple :: forall a b. a -> b -> Tuple a b
tuple = curry id

zip :: forall a b. [a] -> [b] -> [Tuple a b]
zip = zipWith tuple

unzip :: forall a b. [Tuple a b] -> Tuple [a] [b]
unzip = \ts -> case ts of
  t : ts1 -> case unzip ts1 of
    { fst = as, snd = bs } -> tuple (cons t.fst as) (cons t.snd bs)
  [] -> tuple [] []

-- Strings

foreign import member "length" lengthS :: String -> Number

foreign import charAt :: Number -> String -> String

foreign import indexOfS :: String -> String -> Number

foreign import lastIndexOfS :: String -> String -> Number

foreign import localeCompare :: String -> String -> Number

foreign import replace :: String -> String -> String -> String

foreign import sliceS :: Number -> Number -> String -> String

foreign import split :: String -> String -> [String]

foreign import substr :: Number -> Number -> String -> String

foreign import substring :: Number -> Number -> String -> String

foreign import toLower :: String -> String

foreign import toUpper :: String -> String

foreign import trim :: String -> String

-- Regex

foreign import data Regex :: *

foreign import regex :: String -> String -> Regex

foreign import test :: Regex -> String -> Boolean

foreign import match :: Regex -> String -> [String]

foreign import replaceR :: Regex -> String -> String -> String

foreign import search :: Regex -> String -> Number

-- Globals

foreign import nan :: Number

foreign import infinity :: Number

foreign import toExponential :: Number -> String

foreign import toFixed :: Number -> Number -> String

foreign import toPrecision :: Number -> Number -> String

foreign import numberToString :: Number -> String

foreign import isNaN :: Number -> Boolean

foreign import isFinite :: Number -> Boolean

foreign import parseFloat :: String -> Number

foreign import parseInt :: String -> Number

foreign import encodeURIComponent :: String -> String

foreign import decodeURIComponent :: String -> String

foreign import encodeURI :: String -> String

foreign import decodeURI :: String -> String

-- Math

type Math = 
  { abs :: Number -> Number
  , acos :: Number -> Number
  , asin :: Number -> Number
  , atan :: Number -> Number
  , atan2 :: (Number, Number) -> Number
  , aceil :: Number -> Number
  , cos :: Number -> Number
  , exp :: Number -> Number
  , floor :: Number -> Number
  , log :: Number -> Number
  , max :: (Number, Number) -> Number
  , pow :: (Number, Number) -> Number
  , random :: () -> Number
  , round :: Number -> Number
  , sin :: Number -> Number
  , sqrt :: Number -> Number
  , tan :: Number -> Number
  }

foreign import math :: Math


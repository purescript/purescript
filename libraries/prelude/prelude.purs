module Prelude where

  id :: forall a. a -> a
  id = \x -> x

  flip :: forall a b c. (a -> b -> c) -> b -> a -> c
  flip = \f -> \b -> \a -> f a b

  konst :: forall a b. a -> b -> a
  konst = \a -> \b -> a

  (|>) :: forall a b c. (a -> b) -> (b -> c) -> a -> c
  (|>) = \f -> \g -> \a -> g (f a)

  infixr 5 |>

  (<|) :: forall a b c. (b -> c) -> (a -> b) -> a -> c
  (<|) = flip (|>)

  infixr 5 <|

  ($) :: forall a b. (a -> b) -> a -> b
  ($) f x = f x

  infixr 1000 $

module Maybe where

  data Maybe a = Nothing | Just a

  maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
  maybe b _ Nothing = b
  maybe _ f (Just a) = f a

  fromMaybe :: forall a. a -> Maybe a -> a
  fromMaybe a = maybe a Prelude.id

  bindMaybe :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
  bindMaybe m f = maybe Nothing f m

module Either where

  data Either a b = Left a | Right b

  either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
  either f _ (Left a) = f a
  either _ g (Right b) = g b 

  bindEither :: forall e a b. Either e a -> (a -> Either e b) -> Either e b
  bindEither = either (\e _ -> Left e) (\a f -> f a) 

module Arrays where

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

  foreign import length "function length(xs) { \
                        \  return xs.length; \
                        \}" :: forall a. [a] -> Number

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

  infixr 6 :

  (:) :: forall a. a -> [a] -> [a]
  (:) a = concat [a]

  concatMap :: forall a b. [a] -> (a -> [b]) -> [b]
  concatMap [] f = []
  concatMap (a:as) f = f a `concat` concatMap as f

  filter :: forall a. (a -> Boolean) -> [a] -> [a]
  filter _ [] = []
  filter p (x:xs) | p x = x : filter p xs
  filter p (_:xs) = filter p xs

  empty :: forall a. [a] -> Boolean
  empty [] = true
  empty _ = false 

  range :: Number -> Number -> [Number]
  range lo hi = {
      var ns = [];
      for (n <- lo until hi) {
	ns = push ns n;
      }
      return ns;
    }

  zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
  zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
  zipWith _ _ _ = []

  any :: forall a. (a -> Boolean) -> [a] -> Boolean
  any _ [] = false
  any p (a:as) = p a || any p as

  all :: forall a. (a -> Boolean) -> [a] -> Boolean
  all _ [] = true
  all p (a:as) = p a && all p as

module Tuple where

  import Arrays

  type Tuple a b = { fst :: a, snd :: b }

  curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c
  curry f a b = f { fst: a, snd: b }

  uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c
  uncurry f t = f t.fst t.snd

  tuple :: forall a b. a -> b -> Tuple a b
  tuple = curry Prelude.id

  zip :: forall a b. [a] -> [b] -> [Tuple a b]
  zip = zipWith tuple

  unzip :: forall a b. [Tuple a b] -> Tuple [a] [b]
  unzip (t:ts) = case unzip ts of
      { fst = as, snd = bs } -> tuple (t.fst : as) (t.snd : bs)
  unzip [] = tuple [] []

module String where

  foreign import lengthS "function lengthS(s) {\
                         \  return s.length;\
                         \}" :: String -> Number

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

module Regex where

  foreign import data Regex :: *

  foreign import regex :: String -> String -> Regex

  foreign import test :: Regex -> String -> Boolean

  foreign import match :: Regex -> String -> [String]

  foreign import replaceR :: Regex -> String -> String -> String

  foreign import search :: Regex -> String -> Number

module Global where

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

module Math where

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
  
module Eff where

  foreign import data Eff :: # ! -> * -> *

  foreign import retEff "function retEff(a) { return function() { return a; }; }" :: forall e a. a -> Eff e a

  foreign import bindEff "function bindEff(a) { return function(f) { return function() { return f(a())(); }; }; }" :: forall e a b. Eff e a -> (a -> Eff e b) -> Eff e b 

  type Pure a = forall e. Eff e a

  foreign import runPure "function runPure(f) { return f(); }" :: forall a. Pure a -> a

  eff = { ret: retEff, bind: bindEff }

module Errors where

  import Eff

  foreign import data Error :: * -> !

  foreign import throwError "function throwError(e) { return function() { throw e; }; }" :: forall a e r. e -> Eff (err :: Error e | r) a

  foreign import catchError "function catchError(c) { return function(t) { return function() { try { return t(); } catch(e) { return c(e)(); } }; }; }" :: forall e r a. (e -> Eff r a) -> Eff (err :: Error e | r) a -> Eff r a

module IORef where

  import Eff
  
  foreign import data Ref :: !
  
  foreign import data IORef :: * -> *
  
  foreign import newIORef :: forall s r. s -> Eff (ref :: Ref | r) (IORef s)

  foreign import readIORef :: forall s r. IORef s -> Eff (ref :: Ref | r) s

  foreign import writeIORef :: forall s r. IORef s -> s -> Eff (ref :: Ref | r) {}

module Trace where

  import Eff
  
  foreign import data Trace :: !
  
  foreign import trace "function trace(s) { return function() { console.log(s); return {}; }; }" :: forall r. String -> Eff (trace :: Trace | r) {}

  foreign import print "function print(o) { return function() { console.log(JSON.stringify(o)); return {}; }; }" :: forall a r. a -> Eff (trace :: Trace | r) {}

module ST where

  import Eff

  foreign import data ST :: * -> !

  foreign import data STRef :: * -> * -> *

  foreign import newSTRef :: forall a h r. a -> Eff (st :: ST h | r) (STRef h a)

  foreign import readSTRef :: forall a h r. STRef h a -> Eff (st :: ST h | r) a

  foreign import modifySTRef :: forall a h r. (a -> a) -> STRef h a -> Eff (st :: ST h | r) {}

  foreign import runST :: forall a r. (forall h. Eff (st :: ST h | r) a) -> Eff r a


module Prelude where

  flip :: forall a b c. (a -> b -> c) -> b -> a -> c
  flip f b a = f a b

  const :: forall a b. a -> b -> a
  const a _ = a

  on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
  on f g x y = g x `f` g y

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

  instance eqString :: Eq String where
    (==) = Data.Eq.Unsafe.refEq
    (/=) = Data.Eq.Unsafe.refIneq

  instance eqNumber :: Eq Number where
    (==) = Data.Eq.Unsafe.refEq
    (/=) = Data.Eq.Unsafe.refIneq

  instance eqBoolean :: Eq Boolean where
    (==) = Data.Eq.Unsafe.refEq
    (/=) = Data.Eq.Unsafe.refIneq

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

module Data.Eq where

  import qualified Data.Eq.Unsafe as Unsafe

  data Ref a = Ref a

  liftRef :: forall a b. (a -> a -> b) -> Ref a -> Ref a -> b
  liftRef f (Ref x) (Ref y) = f x y

  refEq :: forall a. Ref a -> Ref a -> Boolean
  refEq = liftRef Unsafe.refEq

  refIneq :: forall a. Ref a -> Ref a -> Boolean
  refIneq = liftRef Unsafe.refIneq

  instance eqRef :: Eq (Ref a) where
    (==) = refEq
    (/=) = refIneq
    
module Data.Eq.Unsafe where

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

module Control.Monad.Eff where

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

module Debug.Trace where

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

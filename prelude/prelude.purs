module Prelude where

  flip :: forall a b c. (a -> b -> c) -> b -> a -> c
  flip f b a = f a b

  const :: forall a b. a -> b -> a
  const a _ = a

  asTypeOf :: forall a. a -> a -> a
  asTypeOf x _ = x

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

  infixr 0 $
  infixl 0 #

  ($) :: forall a b. (a -> b) -> a -> b
  ($) f x = f x

  (#) :: forall a b. a -> (a -> b) -> b
  (#) x f = f x

  infixr 6 :

  (:) :: forall a. a -> [a] -> [a]
  (:) = cons

  foreign import cons
    "function cons(e) {\
    \  return function (l) {\
    \    return [e].concat(l);\
    \  };\
    \}" :: forall a. a -> [a] -> [a]

  class Show a where
    show :: a -> String

  foreign import showStringImpl
    "function showStringImpl(s) {\
    \  return JSON.stringify(s);\
    \}" :: String -> String

  instance showString :: Show String where
    show = showStringImpl

  instance showBoolean :: Show Boolean where
    show true = "true"
    show false = "false"

  foreign import showNumberImpl "function showNumberImpl(n) {\
                                \  return n.toString();\
                                \}" :: Number -> String

  instance showNumber :: Show Number where
    show = showNumberImpl

  foreign import showArrayImpl
    "function showArrayImpl (f) {\
    \  return function (xs) {\
    \    var ss = [];\
    \    for (var i = 0, l = xs.length; i < l; i++) {\
    \      ss[i] = f(xs[i]);\
    \    }\
    \    return '[' + ss.join(',') + ']';\
    \  };\
    \}" :: forall a. (a -> String) -> [a] -> String

  instance showArray :: (Show a) => Show [a] where
    show = showArrayImpl show

  infixl 4 <$>

  class Functor f where
    (<$>) :: forall a b. (a -> b) -> f a -> f b

  infixl 4 <*>

  class (Functor f) <= Apply f where
    (<*>) :: forall a b. f (a -> b) -> f a -> f b

  class (Apply f) <= Applicative f where
    pure :: forall a. a -> f a

  liftA1 :: forall f a b. (Applicative f) => (a -> b) -> f a -> f b
  liftA1 f a = pure f <*> a

  infixl 3 <|>

  class Alternative f where
    empty :: forall a. f a
    (<|>) :: forall a. f a -> f a -> f a

  infixl 1 >>=

  class (Apply m) <= Bind m where
    (>>=) :: forall a b. m a -> (a -> m b) -> m b

  class (Applicative m, Bind m) <= Monad m

  return :: forall m a. (Monad m) => a -> m a
  return = pure

  liftM1 :: forall m a b. (Monad m) => (a -> b) -> m a -> m b
  liftM1 f a = do
    a' <- a
    return (f a')

  ap :: forall m a b. (Monad m) => m (a -> b) -> m a -> m b
  ap f a = do
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

  infix 4 ==
  infix 4 /=

  class Eq a where
    (==) :: a -> a -> Boolean
    (/=) :: a -> a -> Boolean

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

  instance eqString :: Eq String where
    (==) = refEq
    (/=) = refIneq

  instance eqNumber :: Eq Number where
    (==) = refEq
    (/=) = refIneq

  instance eqBoolean :: Eq Boolean where
    (==) = refEq
    (/=) = refIneq

  instance eqArray :: (Eq a) => Eq [a] where
    (==) [] [] = true
    (==) (x:xs) (y:ys) = x == y && xs == ys
    (==) _ _ = false
    (/=) xs ys = not (xs == ys)

  data Ordering = LT | GT | EQ

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

  class (Eq a) <= Ord a where
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

  foreign import unsafeCompare
    "function unsafeCompare(n1) {\
    \  return function(n2) {\
    \    return n1 < n2 ? LT : n1 > n2 ? GT : EQ;\
    \  };\
    \}" :: forall a. a -> a -> Ordering
    
  instance ordBoolean :: Ord Boolean where
    compare false false = EQ
    compare false true  = LT
    compare true  true  = EQ
    compare true  false = GT
    
  instance ordNumber :: Ord Number where
    compare = unsafeCompare
    
  instance ordString :: Ord String where
    compare = unsafeCompare
    
  instance ordArray :: (Ord a) => Ord [a] where
    compare (x:xs) (y:ys) = case compare x y of
      EQ -> compare xs ys
      other -> other

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

  infixr 5 <>

  class Semigroup a where
    (<>) :: a -> a -> a

  foreign import concatString
    "function concatString(s1) {\
    \  return function(s2) {\
    \    return s1 + s2;\
    \  };\
    \}" :: String -> String -> String

  instance semigroupString :: Semigroup String where
    (<>) = concatString

  infixr 5 ++

  (++) :: forall s. (Semigroup s) => s -> s -> s
  (++) = (<>)

module Data.Function where

  on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
  on f g x y = g x `f` g y

module Data.Eq where

  data Ref a = Ref a

  liftRef :: forall a b. (a -> a -> b) -> Ref a -> Ref a -> b
  liftRef f (Ref x) (Ref y) = f x y

  instance eqRef :: Eq (Ref a) where
    (==) = liftRef refEq
    (/=) = liftRef refIneq

module Prelude.Unsafe where

  foreign import unsafeIndex
    "function unsafeIndex(xs) {\
    \  return function(n) {\
    \    return xs[n];\
    \  };\
    \}" :: forall a. [a] -> Number -> a

module Control.Monad.Eff where

  foreign import data Eff :: # ! -> * -> *

  foreign import returnE "function returnE(a) {\
                         \  return function() {\
                         \    return a;\
                         \  };\
                         \}" :: forall e a. a -> Eff e a

  foreign import bindE "function bindE(a) {\
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

  instance functorEff :: Functor (Eff e) where
    (<$>) = liftA1

  instance applyEff :: Apply (Eff e) where
    (<*>) = ap

  instance applicativeEff :: Applicative (Eff e) where
    pure = returnE

  instance bindEff :: Bind (Eff e) where
    (>>=) = bindE

  instance monadEff :: Monad (Eff e)

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

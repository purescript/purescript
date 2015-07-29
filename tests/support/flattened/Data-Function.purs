module Data.Function where

import Prelude

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
foreign import mkFn0 :: forall a. (Unit -> a) -> Fn0 a

-- | Create a function of one argument
foreign import mkFn1 :: forall a b. (a -> b) -> Fn1 a b

-- | Create a function of two arguments from a curried function
foreign import mkFn2 :: forall a b c. (a -> b -> c) -> Fn2 a b c

-- | Create a function of three arguments from a curried function
foreign import mkFn3 :: forall a b c d. (a -> b -> c -> d) -> Fn3 a b c d

-- | Create a function of four arguments from a curried function
foreign import mkFn4 :: forall a b c d e. (a -> b -> c -> d -> e) -> Fn4 a b c d e

-- | Create a function of five arguments from a curried function
foreign import mkFn5 :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Fn5 a b c d e f

-- | Create a function of six arguments from a curried function
foreign import mkFn6 :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Fn6 a b c d e f g

-- | Create a function of seven arguments from a curried function
foreign import mkFn7 :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> Fn7 a b c d e f g h

-- | Create a function of eight arguments from a curried function
foreign import mkFn8 :: forall a b c d e f g h i. (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Fn8 a b c d e f g h i

-- | Create a function of nine arguments from a curried function
foreign import mkFn9 :: forall a b c d e f g h i j. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Fn9 a b c d e f g h i j

-- | Create a function of ten arguments from a curried function
foreign import mkFn10 :: forall a b c d e f g h i j k. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Fn10 a b c d e f g h i j k

-- | Apply a function of no arguments
foreign import runFn0 :: forall a. Fn0 a -> a

-- | Apply a function of one argument
foreign import runFn1 :: forall a b. Fn1 a b -> a -> b

-- | Apply a function of two arguments
foreign import runFn2 :: forall a b c. Fn2 a b c -> a -> b -> c

-- | Apply a function of three arguments
foreign import runFn3 :: forall a b c d. Fn3 a b c d -> a -> b -> c -> d

-- | Apply a function of four arguments
foreign import runFn4 :: forall a b c d e. Fn4 a b c d e -> a -> b -> c -> d -> e

-- | Apply a function of five arguments
foreign import runFn5 :: forall a b c d e f. Fn5 a b c d e f -> a -> b -> c -> d -> e -> f

-- | Apply a function of six arguments
foreign import runFn6 :: forall a b c d e f g. Fn6 a b c d e f g -> a -> b -> c -> d -> e -> f -> g

-- | Apply a function of seven arguments
foreign import runFn7 :: forall a b c d e f g h. Fn7 a b c d e f g h -> a -> b -> c -> d -> e -> f -> g -> h

-- | Apply a function of eight arguments
foreign import runFn8 :: forall a b c d e f g h i. Fn8 a b c d e f g h i -> a -> b -> c -> d -> e -> f -> g -> h -> i

-- | Apply a function of nine arguments
foreign import runFn9 :: forall a b c d e f g h i j. Fn9 a b c d e f g h i j -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j

-- | Apply a function of ten arguments
foreign import runFn10 :: forall a b c d e f g h i j k. Fn10 a b c d e f g h i j k -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k

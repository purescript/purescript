module Control.Monad.Eff
  ( Eff
  , Pure
  , runPure
  , untilE, whileE, forE, foreachE
  ) where

import Control.Applicative (class Applicative, liftA1)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad, ap)

import Data.Functor (class Functor)
import Data.Unit (Unit)

-- | The `Eff` type constructor is used to represent _native_ effects.
-- |
-- | See [Handling Native Effects with the Eff Monad](http://www.purescript.org/learn/eff/)
-- | for more details.
-- |
-- | The first type parameter is a row of effects which represents the contexts
-- | in which a computation can be run, and the second type parameter is the
-- | return type.
foreign import data Eff :: # ! -> * -> *

instance functorEff :: Functor (Eff e) where
  map = liftA1

instance applyEff :: Apply (Eff e) where
  apply = ap

instance applicativeEff :: Applicative (Eff e) where
  pure = pureE

foreign import pureE :: forall e a. a -> Eff e a

instance bindEff :: Bind (Eff e) where
  bind = bindE

foreign import bindE :: forall e a b. Eff e a -> (a -> Eff e b) -> Eff e b

instance monadEff :: Monad (Eff e)

-- | The `Pure` type synonym represents _pure_ computations, i.e. ones in which
-- | all effects have been handled.
-- |
-- | The `runPure` function can be used to run pure computations and obtain
-- | their result.
type Pure a = Eff () a

-- | Run a pure computation and return its result.
foreign import runPure :: forall a. Pure a -> a

-- | Loop until a condition becomes `true`.
-- |
-- | `untilE b` is an effectful computation which repeatedly runs the effectful
-- | computation `b`, until its return value is `true`.
foreign import untilE :: forall e. Eff e Boolean -> Eff e Unit

-- | Loop while a condition is `true`.
-- |
-- | `whileE b m` is effectful computation which runs the effectful computation
-- | `b`. If its result is `true`, it runs the effectful computation `m` and
-- | loops. If not, the computation ends.
foreign import whileE :: forall e a. Eff e Boolean -> Eff e a -> Eff e Unit

-- | Loop over a consecutive collection of numbers.
-- |
-- | `forE lo hi f` runs the computation returned by the function `f` for each
-- | of the inputs between `lo` (inclusive) and `hi` (exclusive).
foreign import forE :: forall e. Int -> Int -> (Int -> Eff e Unit) -> Eff e Unit

-- | Loop over an array of values.
-- |
-- | `foreach xs f` runs the computation returned by the function `f` for each
-- | of the inputs `xs`.
foreign import foreachE :: forall e a. Array a -> (a -> Eff e Unit) -> Eff e Unit

module Main where

import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)

class TwoParams a b where
  func :: a -> b

instance equals :: TwoParams a a where
  func a = a
else
instance any :: TwoParams a b where
  func = unsafeCoerce

testEquals :: forall a. a -> a
testEquals = func -- with instance `equals`
testAny :: Int -> Boolean
testAny = func -- with instance `any`

-- `a` and `m a` are never unifiable unless we have infinite types (and of course not)
-- so expected that the instance `any` is chosen.
thisShouldBeCompiled :: forall m a. a -> m a
thisShouldBeCompiled = func

main = log "Done"

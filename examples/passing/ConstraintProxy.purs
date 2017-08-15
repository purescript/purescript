module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Eq (class Eq1)
import Partial.Unsafe (unsafeCrashWith)

test1 :: forall a. Dict (class Show a) -> Dict (class Show a)
test1 d = d

test2 :: forall c. Dict c -> Dict c
test2 d = d

-- Some small error with quantification:

-- test3 :: Dict (class Show (Array _))
-- test3 = showArray

class Lifting c f where
  lift :: forall a. Dict (c a) -> Dict (c (f a))

instance liftingShow :: Lifting (class Show) Array where
  lift = unsafeCrashWith "todo: Need to implement 'with'" 

class Lifted c c1 | c -> c1, c1 -> c1 where
  lower :: forall f a. Dict (c1 f) -> Dict (c a) -> Dict (c (f a))

instance lifted :: Lifted c (class Lifting c) where
  lower _ = unsafeCrashWith "todo: Need to implement 'with'"

main = log "Done"

module Main where

import Prelude
import Control.Monad.Eff.Console (log)

test1 :: (forall a. (a -> a)) -> Number
test1 = \f -> f 0.0

forever :: forall m a b. (forall a b. m a -> (a -> m b) -> m b) -> m a -> m b
forever = \bind action -> bind action $ \_ -> forever bind action

main = log "Done"

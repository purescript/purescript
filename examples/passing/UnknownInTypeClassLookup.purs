module Main where

import Prelude
import Control.Monad.Eff.Console (log)

class EQ a b

instance eqAA :: EQ a a

test :: forall a b. (EQ a b) => a -> b -> String
test _ _ = "Done"

runTest a = test a a

main = log $ runTest 0.0

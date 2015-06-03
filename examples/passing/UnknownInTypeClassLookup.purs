module Main where

import Prelude

class EQ a b

instance eqAA :: EQ a a

test :: forall a b. (EQ a b) => a -> b -> String
test _ _ = "Done"

runTest a = test a a

main = Debug.Trace.trace $ runTest 0.0

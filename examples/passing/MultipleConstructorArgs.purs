module MultipleConstructorArgs where

data P a b = P a b

runP :: forall a b r. (a -> b -> r) -> P a b -> r
runP f (P a b) = f a b

module Main where

import Prelude
import MultipleConstructorArgs
import Global

main = Trace.trace (runP (\s n -> s ++ show n) (P "Test" 1))

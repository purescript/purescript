module Main where

import Prelude

test1 = \_ -> 0.0

test2 = \a b -> a + b + 1.0

test3 = \a -> a

test4 = \(%%) -> 1.0 %% 2.0

test5 = \(+++) (***) -> 1.0 +++ 2.0 *** 3.0

main = Control.Monad.Eff.Console.log "Done"

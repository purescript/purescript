module Main where

import Prelude

data Auto s i o = Auto { state :: s, step :: s -> i -> o }

type SomeAuto i o = forall r. (forall s. Auto s i o -> r) -> r

exists :: forall s i o. s -> (s -> i -> o) -> SomeAuto i o
exists = \state step f -> f (Auto { state: state, step: step })

run :: forall i o. SomeAuto i o -> i -> o
run = \s i -> s (\a -> case a of Auto a -> a.step a.state i)

main = Control.Monad.Eff.Console.log "Done"

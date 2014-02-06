module Auto where

  data Auto s i o = MkAuto { state :: s, step :: s -> i -> o }

  type SomeAuto i o = forall r. (forall s. Auto s i o -> r) -> r

  exists :: forall s i o. s -> (s -> i -> o) -> SomeAuto i o
  exists = \state step f -> f (MkAuto { state: state, step: step })

  run :: forall i o. SomeAuto i o -> i -> o
  run = \s i -> s (\a -> case a of MkAuto a -> a.step a.state i)
    
module Main where

main = Trace.trace "Done"

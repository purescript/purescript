module Main where

import Prelude

class T s m where
  state :: (s -> s) -> m Unit

data S s a = S (s -> { new :: s, ret :: a })

instance st :: T s (S s) where
  state f = S $ \s -> { new: f s, ret: unit }

test1 :: forall r . S { foo :: String | r } Unit
test1 = state $ \o -> o { foo = o.foo ++ "!" }

test2 :: forall m r . (T { foo :: String | r } m) => m Unit
test2 = state $ \o -> o { foo = o.foo ++ "!" }

main = do
  let t1 = test1
  let t2 = test2
  Control.Monad.Eff.Console.log "Done"

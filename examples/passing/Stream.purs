module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

class IsStream el s | s -> el where
  cons :: el -> (Unit -> s) -> s
  uncons :: s -> { head :: el, tail :: s }

data Stream a = Stream a (Unit -> Stream a)

instance streamIsStream :: IsStream a (Stream a) where
  cons x xs = Stream x xs
  uncons (Stream x f) = { head: x, tail: f unit }

test :: forall el s. IsStream el s => s -> s
test s = case uncons s of
           { head, tail } -> cons head \_ -> tail

main :: Eff (console :: CONSOLE) Unit
main = do
  let dones :: Stream String
      dones = cons "Done" \_ -> dones
  log (uncons (test dones)).head

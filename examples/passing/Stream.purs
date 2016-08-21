module Main where

import Prelude

import Control.Monad.Eff.Console

class IsStream el s where
  cons :: el -> (Unit -> s) -> s
  uncons :: s -> { head :: el, tail :: s }

data Stream a = Stream a (Unit -> Stream a)

instance streamIsStream :: IsStream a (Stream a) where
  cons x xs = Stream x xs
  uncons (Stream x f) = { head: x, tail: f unit }

main = do
  let test :: Stream Int
      test = cons 1 \_ -> test
  logShow (uncons test).head

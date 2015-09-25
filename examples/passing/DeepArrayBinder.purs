module Main where

import Prelude
import Control.Monad.Eff
import Test.Assert

data List a = Cons a (List a) | Nil

match2 :: List Number -> Number
match2 (Cons x (Cons y xs)) = x * y + match2 xs
match2 _ = 0.0

main = do
  let result = match2 (Cons 1.0 (Cons 2.0 (Cons 3.0 (Cons 4.0 (Cons 5.0 (Cons 6.0 (Cons 7.0 (Cons 8.0 (Cons 9.0 Nil)))))))))
  assert' "Incorrect result!" (result == 100.0)
  Control.Monad.Eff.Console.log "Done"

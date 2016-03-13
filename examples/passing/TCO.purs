module Main where

import Prelude
import Control.Monad.Eff.Console (print)

main = do
  let f x = x + 1
  let v = 0
  print (applyN 0 f v)
  print (applyN 1 f v)
  print (applyN 2 f v)
  print (applyN 3 f v)
  print (applyN 4 f v)

applyN :: forall a. Int -> (a -> a) -> a -> a
applyN = go id
  where
  go f n _ | n <= 0 = f
  go f n g = go (f >>> g) (n - 1) g


module Main where

f x y z =
  let f 1 2 3 = 1
  in f x z y

main = Debug.Trace.trace $ show $ f 1 3 2

module Main where

test = 
  let f :: Number -> Boolean
      f 0 = false
      f n = g (n - 1)

      g :: Number -> Boolean
      g 0 = true
      g n = f (n - 1)

      x = f 1
  in not x

main = Debug.Trace.print test

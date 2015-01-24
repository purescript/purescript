module Main where

  import Prelude

  f 0 = 0
  f x = g x + 1

  g x = f (x / 2)

  data Even = Zero | Even Odd

  data Odd = Odd Even

  evenToNumber Zero = 0
  evenToNumber (Even n) = oddToNumber n + 1

  oddToNumber (Odd n) = evenToNumber n + 1

  main = Debug.Trace.trace "Done"

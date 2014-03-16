module Main where

  import Prelude

  test1 = \_ -> 0

  test2 = \a b -> a + b + 1

  test3 = \a -> a

  test4 = \(%%) -> 1 %% 2

  test5 = \(+++) (***) -> 1 +++ 2 *** 3

  main = Debug.Trace.trace "Done"

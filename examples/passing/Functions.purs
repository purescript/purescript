module Functions where

  test1 = \ -> 0

  test2 = \() -> 0

  test3 = \a (b, c) d -> a + b + c + d

  test4 = \(a) -> a

  test5 = \(%%) -> 1 %% 2

  test6 = \((+++), (***)) -> 1 +++ 2 *** 3
    
module Main where

main = Trace.trace "Done"

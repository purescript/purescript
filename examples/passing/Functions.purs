module Functions where

  test1 = \_ -> 0

  test2 = \a b c d -> a + b + c + d

  test3 = \a -> a

  test4 = \(%%) -> 1 %% 2

  test5 = \(+++) (***) -> 1 +++ 2 *** 3
    
module Main where

main = Trace.trace "Done"

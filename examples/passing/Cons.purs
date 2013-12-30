module Cons where

  infixr 6 :

  foreign import (:) :: forall a. a -> [a] -> [a]

  test1 = \xs -> 1 : xs

  test2 = 1 : 2 : 3 : []

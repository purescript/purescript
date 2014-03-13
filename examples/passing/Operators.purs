module Operators1 where

  import Prelude ((++))

  (@@) :: forall a b. (a -> b) -> a -> b
  (@@) = \f x -> f x

  foo :: String -> String
  foo = \s -> s
 
  test8 = foo @@ "Hello World"

  bar = \s1 s2 -> s1 ++ s2

module Operators2 where

  import Prelude
  import Operators1 ((@@))

  test1 :: forall n. (Num n) => n -> n -> (n -> n -> n) -> n
  test1 x y z = x * y + z x y

  test2 = (\x -> x.foo false) { foo : \_ -> 1 }

  test3 = (\x y -> x)(1 + 2 * (1 + 2)) (true && (false || false))

  k = \x -> \y -> x

  test4 = 1 `k` 2

  infixl 5 %%

  (%%) :: Number -> Number -> Number
  (%%) x y = x * y + y

  test5 = 1 %% 2 %% 3

  test6 = ((\x -> x) `k` 2) 3

  (<>) = \s1 s2 -> s1 ++ s2

  test7 = "Hello" <> "World!"

  test9 = Operators1.foo @@ "Hello World"

  test10 = "Hello" `Operators1.bar` "World"

  (...) :: forall a. [a] -> [a] -> [a]
  (...) = \as -> \bs -> as

  test11 = [1, 2, 3] ... [4, 5, 6]

  test12 (<%>) a b = a <%> b
  
  test13 = \(<%>) a b -> a <%> b

  test14 :: Number -> Number -> Boolean
  test14 a b = a < b

  test15 :: Number -> Number -> Boolean
  test15 a b = const false $ a `test14` b

module Main where

import Operators1
import Operators2

import Prelude
import Control.Monad.Eff
import Debug.Trace
import Global
import Data.Array

main = 
  print [ show $ test1 1 2 $ \x y -> x + y
        , show test2
        , show test3
        , show test4
        , show test5
        , show test6
        , show test7
        , show test8
        , show test9
        , show test10
        , show test11
        , show (test12 k 1 2)
        , show (test13 k 1 2)
        , show (test14 1 2)
        , show (test15 1 2)
        ]

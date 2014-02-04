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

  test9 = Operators1.foo Operators1.@@ "Hello World"

  test10 = "Hello" `Operators1.bar` "World"

  (...) :: forall a. [a] -> [a] -> [a]
  (...) = \as -> \bs -> as

  test11 = [1, 2, 3] ... [4, 5, 6]

  test12 (<%>) a b = a <%> b

module Main where

import Operators1
import Operators2

import Prelude
import Eff
import Trace
import Global
import Arrays

main = do
  print $ test1 1 2 $ \x y -> x + y
  print test2
  print test3
  print test4
  print test5
  print test6
  print test7
  print test8
  print test9
  print test10
  print test11
  print (test12 k 1 2)


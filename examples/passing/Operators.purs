module Operators1 where

  (@@) :: forall a b. (a -> b) -> a -> b
  (@@) = \f x -> f x

  foo :: String -> String
  foo = \s -> s
 
  test8 = foo @@ "Hello World"

  bar = \s1 s2 -> s1 ++ s2

module Operators2 where

  test1 = \(x, y, z) -> x * y + z(x)(y)

  test2 = (\x -> x.foo false) { foo : \_ -> 1 }

  test3 = (\(x, y) -> x)(1 + 2 * (1 + 2), true && (false || false))

  k = \x -> \y -> x

  test4 = 1 `k` 2

  infixl 5 %%

  (%%) = \x -> \y -> x * y + y

  test5 = 1 %% 2 %% 3

  test6 = ((\x -> x) `k` 2) 3

  (<>) = \s1 -> \s2 -> s1 ++ s2

  test7 = "Hello" <> "World!"

  test9 = Operators1.foo Operators1.@@ "Hello World"

  test10 = "Hello" `Operators1.bar` "World"

  (...) :: forall a. [a] -> [a] -> [a]
  (...) = \as -> \bs -> as

  test11 = [1, 2, 3] ... [4, 5, 6]

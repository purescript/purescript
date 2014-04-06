module Main where

  import Control.Monad.Eff
  import Debug.Trace

  bar :: String -> String -> String
  bar = \s1 s2 -> s1 ++ s2

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

  (<+>) :: String -> String -> String 
  (<+>) = \s1 s2 -> s1 ++ s2

  test7 = "Hello" <+> "World!"

  (@@) :: forall a b. (a -> b) -> a -> b
  (@@) = \f x -> f x

  foo :: String -> String
  foo = \s -> s
 
  test8 = foo @@ "Hello World"

  test9 = Main.foo @@ "Hello World"

  test10 = "Hello" `Main.bar` "World"

  (...) :: forall a. [a] -> [a] -> [a]
  (...) = \as -> \bs -> as

  test11 = [1, 2, 3] ... [4, 5, 6]

  test12 (<%>) a b = a <%> b
  
  test13 = \(<%>) a b -> a <%> b

  test14 :: Number -> Number -> Boolean
  test14 a b = a < b

  test15 :: Number -> Number -> Boolean
  test15 a b = const false $ a `test14` b

  main = do
    let t1 = test1 1 2 (\x y -> x + y)
    let t2 = test2
    let t3 = test3
    let t4 = test4
    let t5 = test5
    let t6 = test6
    let t7 = test7
    let t8 = test8
    let t9 = test9
    let t10 = test10
    let t11 = test11
    let t12 = test12 k 1 2
    let t13 = test13 k 1 2
    let t14 = test14 1 2
    let t15 = test15 1 2
    trace "Done"

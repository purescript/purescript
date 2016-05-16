module Main where

import Prelude
import Other (foo)
import Other as Other
import Control.Monad.Eff
import Control.Monad.Eff.Console

op1 :: forall a. a -> a -> a
op1 x _ = x

infix 4 op1 as ?!

test1 :: forall n. (Semiring n) => n -> n -> (n -> n -> n) -> n
test1 x y z = x * y + z x y

test2 = (\x -> x.foo false) { foo : \_ -> 1.0 }

test3 = (\x y -> x)(1.0 + 2.0 * (1.0 + 2.0)) (true && (false || false))

k = \x -> \y -> x

test4 = 1 `k` 2

op2 :: Number -> Number -> Number
op2 x y = x * y + y

infixl 5 op2 as %%

test5 = 1.0 %% 2.0 %% 3.0

test6 = ((\x -> x) `k` 2.0) 3.0

op3 :: String -> String -> String
op3 = \s1 s2 -> s1 <> s2

infix 4 op3 as <+>

test7 = "Hello" <+> "World!"

op4 :: forall a b. (a -> b) -> a -> b
op4 = \f x -> f x

infix 4 op4 as @@

test8 = foo @@ "Hello World"

test9 = Other.foo @@ "Hello World"

test10 = "Hello" `Other.baz` "World"

op5 :: forall a. Array a -> Array a -> Array a
op5 = \as -> \bs -> as

infix 4 op5 as ...

test11 = [1.0, 2.0, 0.0] ... [4.0, 5.0, 6.0]

test14 :: Number -> Number -> Boolean
test14 a b = a < b

test15 :: Number -> Number -> Boolean
test15 a b = const false $ a `test14` b

test17 :: Number
test17 = negate (-1.0)

test18 :: Number
test18 = negate $ negate 1.0

test19 :: Number
test19 = negate $ negate (-1.0)

main = do
  let t1 = test1 1.0 2.0 (\x y -> x + y)
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
  let t14 = test14 1.0 2.0
  let t15 = test15 1.0 2.0
  let t17 = test17
  let t18 = test18
  let t19 = test19
  log "Done"

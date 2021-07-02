-- @shouldFailWith NoInstanceFound
module Main where

class Foo a b c | a -> b c, b -> a c

bar :: forall a b. Foo String a b => Int -> String
bar _ = ""

test :: String
test = bar 0

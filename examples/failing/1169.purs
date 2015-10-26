-- @shouldFailWith IncorrectConstructorArity 
module Test where

data Outer a = Outer a
            
data Inner a b = Inner a b

test1 :: forall a b. Outer (Inner a b) -> Boolean
test1 (Outer (Inner _)) = true

test2 :: forall a b. Inner a b -> Boolean
test2 (Inner _) = true

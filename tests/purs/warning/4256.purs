-- @shouldWarnWith MissingTypeDeclaration
-- @shouldWarnWith MissingTypeDeclaration
module Main where

addNumberSuffix :: forall a b c d. a -> b -> c -> d -> a
addNumberSuffix a _ _ _ = a

addNumberSuffix' = addNumberSuffix 0

foo :: forall a b c d. a -> b -> c -> d -> a
foo a _ _ _ = a

bar :: forall a b c d. a -> b -> c -> d -> a
bar a _ _ _ = a

baz a x y = bar (foo a 2 3 4) (foo a 2 3 4) (foo x y a a)

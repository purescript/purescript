-- @shouldWarnWith MissingTypeDeclaration
module Main where

addNumberSuffix :: forall a b c d. a -> b -> c -> d -> a
addNumberSuffix a _ _ _ = a

addNumberSuffix' = addNumberSuffix 0

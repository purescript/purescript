-- @shouldFailWith UnusableDeclaration
module Main where

class C a where
  -- type doesn't contain the type class var `a`
  c :: forall a. a


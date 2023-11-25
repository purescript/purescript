-- @shouldWarnWith ShadowedTypeVar
module Main where

class C :: Type -> Constraint
class C a where
  -- The `a` in the type below should refer to the `a`
  -- introduced by the `forall`, not the class head.
  c :: forall a. a

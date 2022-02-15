module Data.Reflectable where

class Reflectable :: forall k. k -> Type -> Constraint
class Reflectable v t | v -> t where
  reflectType :: forall proxy. proxy v -> t

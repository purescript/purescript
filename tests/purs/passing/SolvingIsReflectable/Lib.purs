module Data.Reflectable where

class IsReflectable :: forall k. k -> Type -> Constraint
class IsReflectable v t | v -> t where
  reflectType :: forall proxy. proxy v -> t

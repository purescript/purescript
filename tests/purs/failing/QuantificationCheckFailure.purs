-- @shouldFailWith QuantificationCheckFailureInKind
module Main where

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

data Relate :: forall a (b :: a). a -> Proxy b -> Type
data Relate x y = Relate

-- Inferring and generalizing the kind of `d` such that implicitly generalized
-- variables appear first would result in a reference to `a` before `a` is
-- declared. See "Kind Inference for Datatypes" Section 7.2
data T :: forall (a :: Type) (b :: a) (c :: a) d. Relate b d -> Type
data T a = T

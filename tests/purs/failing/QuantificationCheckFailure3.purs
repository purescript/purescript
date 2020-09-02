-- @shouldFailWith VisibleQuantificationCheckFailureInType
module Main where

foreign import data KProxy :: forall (k :: Type) . k -> Type
foreign import data TProxy :: forall (k :: Type) (t :: k) . KProxy t

type Hmm k = (TProxy :: KProxy k)

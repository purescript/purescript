-- @shouldFailWith CannotDeriveInvalidConstructorArg
module FoldableInstance10 where

import Prelude
import Data.Tuple (Tuple)
import Data.Foldable (class Foldable)

foreign import data Variant :: Row Type -> Type

data Test a = Test (Variant (left :: a, right :: Array a))
derive instance Foldable Test

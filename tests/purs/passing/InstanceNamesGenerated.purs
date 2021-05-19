module Main where

import Effect.Console (log)
import Data.Generic.Rep (class Generic)

-- This file verifies that unnamed instances will produce
-- completely-generated instance names without problems.

class NoTypeParams
instance NoTypeParams


class OneTypeParam a
instance OneTypeParam Boolean


class OneTypeParamChain a
instance OneTypeParamChain Boolean
else instance OneTypeParamChain String


class MultipleTypeParams :: Type -> Type -> Type -> Type -> Type -> Constraint
class MultipleTypeParams a b c d e

instance MultipleTypeParams Boolean Int Number Char String


class MultipleTypeParamsChain :: Type -> Type -> Type -> Type -> Type -> Constraint
class MultipleTypeParamsChain a b c d e

instance MultipleTypeParamsChain Boolean Int Number Char Boolean
else instance MultipleTypeParamsChain Boolean Int Number Char Int
else instance MultipleTypeParamsChain Boolean Int Number Char Number
else instance MultipleTypeParamsChain Boolean Int Number Char Char
else instance MultipleTypeParamsChain Boolean Int Number Char String


class HigherKindedTypeParams :: (Type -> Type) -> (Type -> Type) -> Constraint
class HigherKindedTypeParams f g where
  hktp :: f Int -> g Int -> Int

instance HigherKindedTypeParams Array (Either Int) where
  hktp _ _ = 0


class HigherKindedTypeParamsChain :: (Type -> Type) -> (Type -> Type) -> Constraint
class HigherKindedTypeParamsChain f g where
  hktpChain :: f Int -> g Int -> Int

instance HigherKindedTypeParamsChain Array (Either Int) where
  hktpChain _ _ = 0
else instance HigherKindedTypeParamsChain (Either Int) Array where
  hktpChain _ _ = 0


data CustomKind
foreign import data Constructor1 :: CustomKind
foreign import data Constructor2 :: CustomKind
foreign import data Constructor3 :: CustomKind

class MultipleKindParams :: CustomKind -> Constraint
class MultipleKindParams customKind

instance MultipleKindParams Constructor1


class MultipleKindParamsChain :: CustomKind -> Constraint
class MultipleKindParamsChain customKind

instance MultipleKindParamsChain Constructor1
else instance MultipleKindParamsChain Constructor2
else instance MultipleKindParamsChain Constructor3


data Arrow a b = Foo a b
class ReservedWord a
instance ReservedWord (Arrow a b)
instance ReservedWord ((->) a b)


data GenericFoo = GenericFoo
derive instance Generic GenericFoo _


class OverlappingStillCompiles a
instance OverlappingStillCompiles x
else instance OverlappingStillCompiles x


main = log "Done"

data Either l r = Left l | Right r

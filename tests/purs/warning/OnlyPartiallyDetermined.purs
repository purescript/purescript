-- @shouldWarnWith ShadowedTypeVar
-- @shouldWarnWith OnlyPartiallyDetermined
-- @shouldWarnWith OnlyPartiallyDetermined
-- @shouldWarnWith OnlyPartiallyDetermined
-- @shouldWarnWith OnlyPartiallyDetermined
-- @shouldWarnWith OnlyPartiallyDetermined
-- @shouldWarnWith OnlyPartiallyDetermined
-- @shouldWarnWith OnlyPartiallyDetermined
module Main where

class JustOne (a :: Type) where
  justOne :: String

class JustTwo (a :: Type) (b :: Type) where
  justTwo :: String

class NotBidirectional (a :: Type) (b :: Type) | a -> b where
  notBidirectional :: String

class NotInMember (a :: Type) where
  notInMember :: forall a. a

class JustOne a <= NotInSuperclass a (b :: Type) where
  notInSuperclass :: String

class JustThreeButCIsRequired (a :: Type) (b :: Type) (@c :: Type) where
  justThreeButCIsRequired :: String

class AlmostThere (a :: Type) (b :: Type) (c :: Type)
  | a b -> c
  , a c -> b where
  almostThere :: String

class TypeLevelFunction (a :: Type) (b :: Type) | a -> b

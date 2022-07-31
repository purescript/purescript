-- @shouldWarnWith ShadowedTypeVar
-- @shouldWarnWith OnlyPartiallyDetermined
-- @shouldWarnWith OnlyPartiallyDetermined
-- @shouldWarnWith OnlyPartiallyDetermined
-- @shouldWarnWith OnlyPartiallyDetermined
-- @shouldWarnWith OnlyPartiallyDetermined
-- @shouldWarnWith OnlyPartiallyDetermined
-- @shouldWarnWith OnlyPartiallyDetermined
module Main where

class JustOne (a :: Type)

class JustTwo (a :: Type) (b :: Type)

class NotBidirectional (a :: Type) (b :: Type) | a -> b

class NotInMember (a :: Type) where
  notInMember :: forall a. a

class JustOne a <= NotInSuperclass a (b :: Type)

class JustThreeButCIsRequired (a :: Type) (b :: Type) (@c :: Type)

class AlmostThere (a :: Type) (b :: Type) (c :: Type)
  | a b -> c
  , a c -> b  

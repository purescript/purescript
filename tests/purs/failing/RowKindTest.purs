-- @shouldFailWith KindsDoNotUnify
module RowKindTest where

import Type.Proxy (Proxy(..))

foreign import data Foo :: Record (a :: Type, b :: Type)
foreign import data Bar :: Record (a :: Int, c :: Type, d :: Type) -> Type

something :: Proxy (Bar Foo)
something = Proxy

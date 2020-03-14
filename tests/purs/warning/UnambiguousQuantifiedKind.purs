-- @shouldWarnWith MissingTypeDeclaration
module Main where

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

-- Should not trigger a warning
test1 :: forall k (a :: k). Proxy a
test1 = Proxy

-- Should trigger a warning
test2 = 42

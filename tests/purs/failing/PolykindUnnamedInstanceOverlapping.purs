-- @shouldFailWith OverlappingInstances
module Main where

data Proxy a = Proxy

class ShowP a where
  showP :: a -> String

instance ShowP (Proxy ((a) :: k)) where
  showP _ = "Type"

instance ShowP (Proxy ((a) :: k)) where
  showP _ = "Type"

-- @shouldFailWith OverlappingInstances
module Main where

data Proxy a = Proxy

class ShowP a where
  showP :: a -> String

instance test1 :: ShowP (Proxy ((a) :: k)) where
  showP _ = "Type"

instance test2 :: ShowP (Proxy ((a) :: k)) where
  showP _ = "Type"

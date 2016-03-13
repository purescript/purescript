module Clash (module Clash1) where

import Clash1 as Clash1
import Clash2 as Clash2

module Clash1 (module Clash1a) where

import Clash1a

module Clash1a where

value :: Int
value = 0

type Type = Int

class TypeClass a where
  typeClassMember :: a

module Clash2 (module Clash2a) where

import Clash2a

module Clash2a where

value :: String
value = "hello"

type Type = String

class TypeClass a b where
  typeClassMember :: a -> b

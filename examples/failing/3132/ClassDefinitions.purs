module ClassDefinitions 
  ( class C2
  , class C3
  ) where

import Prelude

class C1
instance inst1 :: C1

class C1 <= C2 a

class (C2 a) <= C3 a b

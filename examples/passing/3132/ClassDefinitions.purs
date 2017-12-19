module ClassDefinitions 
  ( class Two
  , dos
  , class Tri
  , tres
  ) where

import Prelude

class Zero where
  zilch :: Boolean

class One where
  uno :: Int -> Number
  ein :: Number -> Boolean -> Int

class One <= Two a where
  dos :: a -> Int

class (Two a, Zero) <= Tri b a where
  tres :: a -> b

instance inst0 :: Zero where
  zilch = true

instance inst1 :: One where
  uno _ = 1.23
  ein _ true = 321
  ein _ _ = 123

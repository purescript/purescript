module ClassDefinitions 
  ( class Two
  , dos
  , class Quad
  , quattro
  ) where

import Prelude

class Zero where
  zilch :: Boolean

class One where
  uno :: Int -> Number
  ein :: Number -> Boolean -> Int

class One <= Two a where
  dos :: a -> Int

class (Zero, Two a) <= Quad a b where
  quattro :: b -> a

instance inst0 :: Zero where
  zilch = true

instance inst1 :: One where
  uno _ = 1.23
  ein _ true = 321
  ein _ _ = 123

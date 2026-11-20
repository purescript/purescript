module Main where

import Prelude
import Effect.Console (log)

data ℝ² = V Number Number

instance Show ℝ² where
  show (V x y) = "V " <> show x <> " " <> show y

v :: ℝ²
v = V 1.0 2.0

class Describe a where
  describe :: a -> String

instance Describe ℝ² where
  describe _ = "ℝ²"

main :: Effect Unit
main = do
  log (show v)
  log (describe v)

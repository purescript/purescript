module Data.Void (Void, absurd) where

import Data.Show (class Show)

newtype Void = Void Void

instance showVoid :: Show Void where
  show = absurd

absurd :: forall a. Void -> a
absurd a = spin a
  where
  spin (Void b) = spin b

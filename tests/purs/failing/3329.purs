-- @shouldFailWith NoInstanceFound
module Main where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

class Inject f g where
  inj :: f -> g
  prj :: g -> Maybe f

instance injectRefl :: Inject x x where
  inj x = x
  prj x = Just x
else instance injectLeft :: Inject l (Either l r) where
  inj x = Left x
  prj (Left x) = Just x
  prj _ = Nothing
else instance injectRight :: Inject x r => Inject x (Either l r) where
  inj x = Right (inj x)
  prj (Right x) = prj x
  prj _ = Nothing

injR :: forall f g. g -> Either f g
injR = inj

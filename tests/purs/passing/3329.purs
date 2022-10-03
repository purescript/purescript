module Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

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

injL :: forall f g. f -> Either f g
injL = inj

main :: Effect Unit
main = log "Done"
  where
  testInjLWithUnknowns a = case inj a of
    Left a' -> a'
    Right _ -> a

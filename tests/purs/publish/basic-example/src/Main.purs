module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Newtype (class Newtype, un)

newtype Target = Target String

derive instance newtypeTarget :: Newtype Target _

greetingTarget :: Target
greetingTarget = Target "world"

main :: Effect Unit
main = log ("hello, " <> un Target greetingTarget <> "!")

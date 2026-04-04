module Main where

import Prelude
import Data.Monoid.Additive (Additive(..))
import Effect.Console (log)
import Test.Assert (assert)

newtype Wrapped a = Wrapped a

instance Show a => Show (Wrapped a) where
  show (Wrapped x) = show x

type WrappedString = Wrapped String

-- Attached derive via with parens
newtype MyInt = MyInt Int
  derive (Show) via (Wrapped Int)

-- Attached derive via with type synonym
newtype Name = Name String
  derive (Show) via WrappedString

-- Attached derive via with Semigroup/Monoid
newtype Score = Score Int
  derive newtype (Eq, Ord, Show)
  derive (Semigroup, Monoid) via (Additive Int)

-- Attached derive mixed with standalone derive
data Color = Red | Green | Blue
  derive (Eq)

derive instance Ord Color

-- via still works as identifier and record label
via :: Int
via = 42

viaRecord :: { via :: Int }
viaRecord = { via: 1 }

-- via as function parameter
addVia :: Int -> Int
addVia via = via + 1

-- via in pattern match
matchVia :: Int -> Int
matchVia = case _ of
  via -> via * 2

-- via as type alias name
type Via = Int

useVia :: Via
useVia = 99

main = do
  assert $ show (MyInt 42) == "42"
  assert $ show (Name "hello") == "\"hello\""
  assert $ Score 1 <> Score 2 == Score 3
  assert $ mempty == Score 0
  assert $ Red == Red
  assert $ Red < Green
  assert $ via == 42
  assert $ viaRecord.via == 1
  assert $ addVia 10 == 11
  assert $ matchVia 5 == 10
  assert $ (useVia :: Via) == 99
  log "Done"

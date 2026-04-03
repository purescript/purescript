module Main where

import Prelude
import Effect.Console (log)
import Test.Assert (assert)

-- A simple newtype wrapper for deriving via
newtype Wrapped a = Wrapped a

-- Show instance for Wrapped that delegates to the underlying type
instance Show a => Show (Wrapped a) where
  show (Wrapped x) = show x

-- Our target type
newtype MyInt = MyInt Int

-- Derive Show for MyInt via (Wrapped Int)
derive via (Wrapped Int) instance Show MyInt

-- Test that 'via' is still usable as a regular identifier
via :: Int
via = 42

viaRecord :: { via :: Int }
viaRecord = { via: 1 }

-- A type synonym used as a via type
type WrappedString = Wrapped String

newtype Name = Name String

derive via WrappedString instance Show Name

main = do
  assert $ show (MyInt 42) == "42"
  assert $ show (Name "hello") == "\"hello\""
  assert $ via == 42
  assert $ viaRecord.via == 1
  log "Done"

module ReadShow where

  foreign import read :: forall a. String -> a

  foreign import show :: forall a. a -> String

  test = \x -> show (read x)

module ExternData where

  foreign import data IO :: * -> *

  foreign import fmap :: forall a b. IO a -> (a -> b) -> IO b

  foreign import return :: forall a. a -> IO a

  foreign import bind :: forall a b. IO a -> (a -> IO b) -> IO b

  foreign import showMessage :: String -> IO { }

  foreign import prompt :: IO String

  main = \_ -> prompt `bind` \s -> showMessage s
    
module Main where

main = Debug.Trace.trace "Done"

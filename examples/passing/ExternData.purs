module Main where

  foreign import data IO :: * -> *

  foreign import bind "function bind() {}" :: forall a b. IO a -> (a -> IO b) -> IO b

  foreign import showMessage "function showMessage() {}" :: String -> IO { }

  foreign import prompt "function prompt() {}" :: IO String

  test _ = prompt `bind` \s -> showMessage s

  main = Debug.Trace.trace "Done"

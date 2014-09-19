module Main where

foreign import foo """
  function foo(s) {
    return s;
  }
""" :: String -> String

bar :: String -> String
bar _ = foo "test"

main = Debug.Trace.trace "Done"

module Main where

type X r = { | r }

foreign import x "var x = {};" :: forall r. X r

blah :: forall r. X r -> X r
blah x = x

test = blah x
  { baz = "blah"
  }

main = Debug.Trace.trace "Done"

module Main where

  o :: { type :: String }
  o = { type: "Done" }

  p = o { type = o.type }

  f :: forall r. { type :: String | r } -> String
  f { type = a } = a

  main = Debug.Trace.trace p.type

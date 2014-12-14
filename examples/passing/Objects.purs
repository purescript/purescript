module Main where

  import Prelude

  test = \x -> x.foo + x.bar + 1

  append = \o -> { foo: o.foo, bar: 1 }

  apTest = append({foo : "Foo", baz: "Baz"})

  f = (\a -> a.b.c) { b: { c: 1, d: "Hello" }, e: "World" }

  g = (\a -> a.f { x: 1, y: "y" }) { f: \o -> o.x + 1 }

  typed :: { foo :: Number }
  typed = { foo: 0 }

  test2 = \x -> x."!@#"

  test3 = typed."foo"

  test4 = test2 weirdObj
    where
    weirdObj :: { "!@#" :: Number }
    weirdObj = { "!@#": 1 }

  test5 = case { "***": 1 } of
    { "***" = n } -> n

  test6 = case { "***": 1 } of
               { "***": n } -> n

  test7 {a:    snoog , b     : blah } = blah
    
  main = Debug.Trace.trace "Done"

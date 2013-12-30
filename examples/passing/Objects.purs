module Objects where

  test = \x -> x.foo + x.bar

  append = \o -> { foo: o.foo, bar: 1 }

  apTest = append({foo : "Foo", baz: "Baz"})

  f = (\a -> a.b.c) { b: { c: 1, d: "Hello" }, e: "World" }

  g = (\a -> a.f { x: 1, y: "y" }) { f: \o -> o.x + 1 }

  typed :: { foo :: Number }
  typed = { foo: 0 }

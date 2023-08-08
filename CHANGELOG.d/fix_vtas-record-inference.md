* Infer types using VTA inside a record

  Previously, `useShow` would fail to compile
  because the `v` type variable would not be inferred
  to `String`. Now the below code compiles:

  ```purs
  reflect :: forall @t v . Reflectable t v => v
  reflect = reflectType (Proxy @t)

  use :: String
  use = show { asdf: reflect @"asdf" }
  ```


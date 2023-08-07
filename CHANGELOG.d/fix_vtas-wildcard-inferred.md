* Stop emitting warnings for wildcards in Visible Type Applications

  Previously, the below usage of a wildcard (i.e. `_`) would
  incorrectly cause the compiler to emit a warning.
  
  ```purs
  f :: forall @a. a -> a
  f = identity

  x :: { x :: Int }
  x = f @{ x :: _ } { x: 42 }
  ```

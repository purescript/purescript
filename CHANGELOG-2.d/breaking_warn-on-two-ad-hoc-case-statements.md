* Warn on ad-hoc non-single-line case expression syntax

  The following code will now produce a compiler warning.
  These were originally supported to ease the migration
  to the new CST parser.

  ```purescript
  -- before: `arg` isn't indented "past" the `Foo arg` binder
  case foo of Foo arg ->
    arg
  -- after
  case foo of Foo arg ->
                foo
  ```

  Dropping the above syntax make case expressions more similar to how `let` bindings work:
  ```purescript
  let ok = 1
  let
    ok = 1
  let ok =
        1
  let notOk =
    1
  ```
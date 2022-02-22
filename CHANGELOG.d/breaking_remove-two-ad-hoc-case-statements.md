* Remove ad-hoc same-line single-branch case statements

  The following code will no longer compile. These were originally
  supported to ease the migration to the new CST parser.

  ```purescript
  -- before
  case foo of Foo arg -> foo
  -- after
  case foo of
    Foo arg -> foo
  ```

  ```purescript
  -- before
  case foo of Foo arg | Wrapped val <- arg -> val
  -- after
  case foo of
    Constructor arg | Wrapped val <- arg -> val
  ```
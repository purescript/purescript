* Make `FromJSON` instance for `Qualified` backwards compatible

  Prior to #4293, `Qualified` was encoded to JSON such that

  ```haskell
  >>> encode $ Qualified Nothing "foo"
  [null,"foo"]
  >>> encode $ Qualified (Just $ ModuleName "A") "bar"
  ["A","bar"]
  ```

  The type of `Qualified` has changed so that `null` no longer appears in JSON output, but for sake of backwards-compatibility with JSON that was produced prior to those changes (pre-`v0.15.2`), we need to accept `null`, which will be interpreted as `Qualified ByNullSourcePos`.

* Defer monomorphization for data constructors

  In `0.15.4` and earlier, the compiler monomorphizes type
  constructors early, yielding the following type:

  ```purs
  > :t Nothing
  forall (a1 :: Type). Maybe a1

  > :t { a : Nothing }
  forall (a1 :: Type).
    { a :: Maybe a1
    }
  ```

  With this change, the monomorphization introduced in
  [#835](https://github.com/purescript/purescript/pull/835) is
  deferred to only when it's needed, such as when constructors are
  used as values inside of records.

  ```purs
  > :t Nothing
  forall a. Maybe a

  > :t { a : Nothing }
  forall (a1 :: Type).
    { a :: Maybe a1
    }
  ```

  Also as a consequence, record updates should not throw
  `ConstrainedTypeUnified` in cases such as:

  ```purs
  v1 :: { a :: Maybe Unit }
  v1 = { a : Just Unit }

  v2 :: { a :: Maybe Unit }
  v2 = let v3 = v1 { a = mempty } in v3
  ```

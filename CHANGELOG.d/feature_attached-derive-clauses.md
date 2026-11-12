* Add attached `derive` clauses to `data` and `newtype` declarations (#4594 by @i-am-the-slime)

  Derivable instances can now be declared directly after a type declaration:

  ```purescript
  data Color = Red | Green | Blue
    derive (Eq, Ord)

  newtype Name = Name String
    derive (Eq, Ord)

  data Box a = Empty | Full a
    derive (Functor)
  ```

  Attached clauses support the compiler's built-in deriving mechanisms and can
  be used alongside standalone `derive instance` declarations. They do not
  support `derive newtype`.

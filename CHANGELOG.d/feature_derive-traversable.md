* Enable the compiler to derive `Traversable` instances

  The compiler can now automatically derive a `Traversable` instance for types,
  so long as
  1. All type constructors that use the type variable being folded over
     (e.g. `a` below) **must** appear in the rightmost position that type constructor.
     Any other positions will be considered invalid.
  2. Any higher-kinded type constructor must have a `Foldable` instance. By implication,
     this excludes the `Function` type.

  The examples below indicate valid usages (via `✓`) and invalid usages (via `⨯`) of 
  a type variable `a` used in various places below.
  If the data constructors were refactored so as to remove the invalid usages,
  the compiler can derive a `Traversable` instance for it

  ```purs
  data X f a
    = X0 Int
    --         - although no `a` appears in this data constructor
    --           it doesn't break either of the rules above
    | X1 a a a
    --   ✓ ✓ ✓ - because each `a` is in the rightmost position
    --               due to being a top-level value in the constructor
    | X2 (f a)
    --      ✓ - because the `a` is in the rightmost position of `f`
    | X3 (Tuple a Int)
    --          ⨯ - because the `a` is not in the rightmost position of `Tuple`
    | X4 (Tuple a a)
    --          ⨯ ✓
    | X5 { foo :: a, bar :: f a, baz :: Tuple Int a }
    --            ✓           ✓                   ✓ - records are supported
    | X6 { one :: { two :: { three :: a } } }
    --                                ✓ - even nested ones
  ```

  Records are traversed in the alphabetical ordering of their labels,
  not their definition order. For example, a record defined like
  ```purs
  type Foo a =
    { m :: a
    , f :: a
    , c :: a
    , g :: a
    }
  ```

  will be traversed in a `cfgm` order, not the `mfcg` order.

  Given a data type like the following...

  ```purs
  data M f a
    = M0
    | M1 a (Array a)
    | M2 Int
    | M3 (f a)
    | M4 { a :: a
         , zArrayA :: Array a
         , nested :: { a :: a }
         , fa :: f a
         , ignore :: Int
         , arrayIgnore :: Array Int
         , fIgnore :: f Int
         }

  derive instance Functor f => Functor (M f)
  derive instance Foldable f => Foldable (M f)
  derive instance Traversable f => Traversable (M f)
  ```

  Something like the following will be generated:

  ```purs
  instance Traversable f => Traversable (M f) where
    traverse f z = case _ of
      M0 -> pure M0
      M1 a arr -> (\a' arr' -> M1 a' arr') <$> f a <*> traverse f arr
      M2 i -> pure $ M2 i
      M3 fa -> (\fa' -> M3 fa') <$> traverse f fa
      M4 r ->
        (\a' fa' nestedA' zArrayA' -> 
          M4 $ r { a = a', fa = fa', nested = { a = nestedA' }, zArrayA = zArrayA' }
        )
        <$> f r.a
        <*> traverse f r.fa
        <*> traverse f r.nested.a
        <*> traverse f r.zArrayA

    sequence t = traverse identity t
  ```

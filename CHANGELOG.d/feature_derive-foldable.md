* Enable the compiler to derive `Foldable` instances

  The compiler can now automatically derive a `Foldable` instance for types,
  so long as
  1. All type constructors that use the type variable being folded over
     (e.g. `a` below) **must** appear in the rightmost position that type constructor.
     Any other positions will be considered invalid.
  2. Any higher-kinded type constructor must have a `Foldable` instance. By implication,
     this excludes the `Function` type.

  The examples below indicate valid usages (via `✓`) and invalid usages (via `⨯`) of 
  a type variable `a` used in various places below.
  If the data constructors were refactored so as to remove the invalid usages,
  the compiler can derive a `Foldable` instance for it

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

  Records are folded in the alphabetical ordering of their labels,
  not their definition order. For example, a record defined like
  ```purs
  type Foo a =
    { m :: a
    , f :: a
    , c :: a
    , g :: a
    }
  ```

  will be folded in a `cfgm` order, not the `mfcg` order.

  Given a data type like the following...

  ```purs
  data M f a
    = M0
    | M1 a (Array a)
    | M2 Int
    | M3 (f a)
    | M4 { a :: a, x :: Array a, fa :: f a
         , ignore :: Int, no :: Array Int, nope :: f Int
         , nested :: { anotherOne :: a }
         }

  derive instance Foldable f => Foldable (M f)
  ```

  Something like the following will be generated:
  ```purs
  instance Foldable f => Foldable (M f) where
    foldl f z = case _ of
      M0 -> z
      M1 a arr -> foldl f (f z a) arr
      M2 _ -> z
      M3 fa -> foldl f z fa
      M4 rec ->
        foldl f (foldl f (foldl f (f z rec.a) rec.fa) rec.nested.anotherOne) rec.x
    foldr f z = case _ of
      M0 -> z
      M1 a arr -> f a (foldr f z arr)
      M2 _ -> z
      M3 fa -> foldr f z fa
      M4 rec ->
        f (foldr f (foldr f (f z rec.x) rec.nested.anotherOne) rec.fa) rec.a
    foldMap f = case _ of
      M0 -> mempty
      M1 a arr -> f a <> foldMap f arr
      M2 _ -> mempty
      M3 fa -> foldMap f fa
      M4 rec -> f rec.a <> foldMap f rec.fa <> foldMap f rec.nested.anotherOne <> foldMap rec.x
  ```
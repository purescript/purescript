* Drop `UnusableDeclaration` error

  Previously, the following type class would be invalid
  because there was no way for the compiler to infer
  which type class instance to select because
  the type variable in the class head `a` was
  not mentioned in `bar`'s type signature:
  
  ```purs
  class Foo a where
    bar :: Int
  ```

  With the advent of Visible Type Applications (VTAs), 
  one can inform the compiler via VTAs:
  
  ```purs
  class Foo a where bar :: Int
  instance Foo String where bar = 0
  someInt = bar @String -- uses `String` instance
  ```

  However, failing to provide a VTA to direct the instance selection
  will still produce an `InstanceNotFound` error. Given the following
  code

  ```purs
  class Single tyVarDoesNotAppearInBody where 
    useSingle :: Int

  single :: Int
  single = useSingle
  ```
  
  The error reported for `useSingle` will be:
  
  ```
  No type class instance was found for

    Main.Single t0

  The instance head contains unknown type variables. Consider using visible type application(s):
  
    useSingle @tyVarDoesNotAppearInBody
  ```

  For a multiparameter typeclass with functional dependencies...
  
  ```purs
  class Multi a b c d e f | a c -> d f, b c -> a d where
    useMulti :: Int
  
  multi = useMulti
  ```

  ...each possible option is shown. Type variables determined by others
  that don't affect the order (i.e. all determined args to the right of
  the last undetermined arg) will be omitted.
  ```
  useMulti @a @b @c @_ @e     -- `f` ommitted; `a` and `c` determine it
  useMulti @_ @b @c @_ @e @f  -- `f` included; `b` and `c` don't determine it
  ```

* Replace `UnusableDeclaration` with updated `NoInstanceFound`

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
  someInt = bar @String -- use the `String` instance
  ```

  However, failing to provide a VTA to direct the instance selection
  will still produce an `InstanceNotFound` error, but this error 
  has been updated to note which type variables in the class head
  must be specified via Visible Type Applications. Given the following code

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

  The instance head contains unknown type variables.
  

  Note: The following type class members found in the expression require specifying their corresponding type class' type variables by using Visible Type Applications (e.g. tyClassMember @Int).
    Main.useSingle
      tyNotAppearInBody
  ```

  For a multiparameter typeclass with functional dependencies...
  
  ```purs
  class Multi a b c d e f | a c -> d f, b c -> a d where
    useMulti :: Int
  
  multi = useMulti
  ```

  ...the "Note" part is updated to read
  ```
  Note: The following type class members found in the expression require specifying their corresponding type class' type variables by using Visible Type Applications (e.g. tyClassMember @Int).
  Main.useMulti
    b, c, e
  ```

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

  With the advent of Visible Type Applications, 
  one can inform the compiler via VTAs:
  
  ```purs
  someInt = bar @Int -- :: Int
  ```

  However, failing to provide a VTA to direct the instance selection
  will still produce an `InstanceNotFound` error:

  ```purs
  someInt = bar -- Could not find instance for `Foo t0`
  ```

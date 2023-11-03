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

  The recently-added visible type applications (VTAs)
  can now be used to guide the compiler in such cases:
  
  ```purs
  class Foo a where bar :: Int
  instance Foo String where bar = 0
  someInt = bar @String -- use the `String` instance
  ```

  Without VTAs, the compiler
  will still produce an `InstanceNotFound` error, but this error 
  has been updated to note which type variables in the class head
  can only be disambiguated via visible type applications 
  (except for one situation involving type synonyms).
  Given the following code

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
  

  Note: The following type class members found in the expression require visible type applications 
  to be unambiguous (e.g. tyClassMember @Int).
    Main.useSingle
      tyNotAppearInBody
  ```

  For a multiparameter typeclass with functional dependencies...
  
  ```purs
  class MultiFdBidi a b | a -> b, b -> a where
    useMultiFdBidi :: Int

  multiFdBidi :: Int
  multiFdBidi = useMultiFdBidi
  ```

  ...the "Note" part is updated to read
  ```
  Note: The following type class members found in the expression require visible type applications 
  to be unambiguous (e.g. tyClassMember @Int).
    Main.useMultiFdBidi
      One of the following sets of type variables:
        a
        b
  ```

  In both cases above, the `NoInstanceFound` error will not include 
  the VTA-required-args info when the type class member's type signature uses a type synonym.
  For example,
  ```purs
  type Synonym = Array
  class MyClass a b c where
    tyClassMember :: Synonym a
  ```

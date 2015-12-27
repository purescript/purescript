This error occurs when an instance is declared outside of the module that declares the class it is for, and also none of the instance's types are declared in the same module.

For example:

``` purescript
module A where

  class SomeClass a where
    someAction :: a -> a

module B where

  data SomeData = SomeData

module C where

  import A
  import B

  instance someInstance :: SomeClass SomeData where
    someAction d = d
```

`someInstance` is an orphan here as it is defined in a module separate from both `SomeData` and `SomeClass`.

In a case where a class has multiple type variables (`class SomeClass a b c ...`) and an instance is being declared in a different module, the instance only needs to provide one type from the current module for the instance to not be considered an orphan.

Orphan instances are disallowed as they can cause conflicts in instance resolution in unrelated parts of the codebase, and can result in situations where you need odd imports like `import SomeModule ()` to just bring the instances into scope - with this restriction, empty-importing a module is never necessary.
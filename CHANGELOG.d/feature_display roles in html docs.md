* Display role annotations in HTML docs

  Previously, the HTML docs would not indicate which types could be safely
  coerced and which could not:

  ```purescript
  -- cannot be coerced
  data Foo1 a = Foo1 a
  type role Foo1 nominal

  -- can be coerced
  data Foo2 a = Foo2
  type role Foo2 phantom

  -- can be coerced in some contexts
  data Foo3 a = Foo3 a
  type role Foo3 representational
  ```

  The HTML docs now display the role annotations either explicitly
  declared by the developer or those inferred by the compiler.

  Since role annotations are an advanced feature and since most type
  parameters' roles are the `representational` role, the `phantom` and
  `nominal` role annotations are displayed in documentation whereas the
  `representational` role is not, similar to "uninteresting" kind signatures.

  Lastly, FFI declarations like below...

  ```purescript
  foreign import data Foo :: (Type -> Type) -> Type
  type role Foo nominal
  ```

  ...will be rendered as though they are data declarations:

  ```purescript
  data Foo :: (Type -> Type) -> Type
  data Foo t0
  type role Foo nominal
  ```

  One can distinguish FFI declarations with roles separately from normal `data`
  declarations that have roles based on the name of the type parameters. Since FFI declarations' type parameters are implicit and thus unnamed, the compiler will generate their name: `t0`, `t1`, ..., `tN` where `N` is a zero-based
  index of the type parameter.

  Note: the resulting documentation will display the roles, but the roles
  will not be selectable when selecting the type in case one wants to
  copy-paste the type into source code.

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
  `representational` role is not, similar to "unintersting" kind signatures.

  Note: the resulting documentation will display the roles, but the roles
  will not be selectable when selecting the type in case one wants to
  copy-paste the type into source code.

* Allow parentheses around operators when defining aliases

  The compiler now allows operators in alias declarations to be wrapped in parentheses,
  similar to how custom operators would be defined in languages like Haskell or OCaml.

  ```purs
  pairUp :: forall a b. a -> b -> { a :: a, b :: b }
  pairUp a b = { a, b }

  infixr 4 pairUp as (/\)
  ```

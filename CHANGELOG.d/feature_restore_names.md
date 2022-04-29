* Restore names of quantified variables during generalization

  This makes the compiler aware of the names of quantified variables
  instantiated into unification variables, such that when the latter
  is generalized, semantic information is restored. For example:

  ```purs
  addNumberSuffix :: forall a b c d. a -> b -> c -> d -> a
  addNumberSuffix a _ _ _ = a
  
  addNumberSuffix' = addNumberSuffix 0
  ```

  Previously, inferring top-level declarations without type signatures
  would use `t` suffixed with an integer for type variables.

  ```purs
  forall t6 t7 t8. t6 -> t7 -> t8 -> Int
  ```

  Now, the inferred type would refer back to their original names.

  ```purs
  forall b6 c7 d8. b6 -> c7 -> d8 -> Int
  ```

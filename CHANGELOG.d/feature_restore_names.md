# Restore names of quantified variables during generalization

This makes the compiler aware of the names of quantified variables
instantiated into unification variables, such that when the latter
is generalized, semantic information is restored. For example:

```hs
addNumberSuffix :: forall a b c d. a -> b -> c -> d -> a
addNumberSuffix a _ _ _ = a

addNumberSuffix' = addNumberSuffix 0
```

This now generalizes with the original names suffixed with a number
instead of just being replaced by a `t`.

```hs
Warning found:
in module Main
at Main.purs:6:1 - 6:37 (line 6, column 1 - line 6, column 37)

  No type declaration was provided for the top-level declaration of addNumberSuffix'.
  It is good practice to provide type declarations as a form of documentation.
  The inferred type of addNumberSuffix' was:

    forall b6 c7 d8. b6 -> c7 -> d8 -> Int


in value declaration addNumberSuffix'

See https://github.com/purescript/documentation/blob/master/errors/MissingTypeDeclaration.md for more information,
or to contribute content related to this warning.
```

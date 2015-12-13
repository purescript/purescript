```
Error checking that type
  Control.Monad.Eff.Eff (assert :: Test.Assert.ASSERT | _0) Prelude.Unit
subsumes type
  Control.Monad.Eff.Eff (a :: Test.Assert.ASSERT | e0) Prelude.Unit
Error at /.../Main.purs line 7, column 3 - line 7, column 10:
  Cannot unify type
    (assert :: Test.Assert.ASSERT | _0)
  with type
    (a :: Test.Assert.ASSERT | e0)
```

Even though the type of the effect (`Test.Assert.ASSERT`) is correct, the name does not match. The effect name *must* match. So to fix this error, change `a` to `assert`. 

*When using Halogen*

If you are seeing this error in relation to the definition of a component try checking the type signatures  of the component and child component's slot addresses, query types and state types against the examples in the purescript halogen repository.
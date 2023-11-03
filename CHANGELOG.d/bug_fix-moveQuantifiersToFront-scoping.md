* Fix scoping issues in `moveQuantifiersToFront`

As a side effect of replacing `UnusableDeclaration` with
an updated `NoInstanceFound` error, a bug appeared in how
scoping is handled when constraints are involved.

```purs
--        | a0
class Foo a where
--              | a1
  foo :: forall a. a
```
Before this fix, `foo`'s type signature was being transformed to
`foo :: forall @a a. Foo a => a`
where two type variables with the same identifier 
are present rather than the correct signature of
`foo :: forall @a0. Foo a0 => (forall a1. a1)`.

With this fix, the above type class declaration
will now compile and trigger a `ShadowedName` 
warning since the type class member's `a` 
(i.e. `a1` above) shadows the type class head's `a` 
(i.e. `a0` above).

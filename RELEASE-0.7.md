## 0.6.1 Changes

### Breaking Changes

- The body of a guarded expression must now be indented _past the guard_. For example, this is valid:

```haskell
positive n | n > 0 = true
positive _ = false
```

but this is not:

```haskell
positive n | n > 0
  = true
positive _ = false
```

### New Features

- Type wildcards are now supported (#287, @paf31)

### Enhancements

- Allow unquoted keywords as key names in record literals (#606, @michaelficarra)
- Import instances when referencing qualified values (#667, @garyb)
- Multiple guard clauses are now supported (#294, @paf31)
- Type check let declarations immediately in psci (#615, @garyb)

## 0.6.2 Changes

### Breaking Changes

-

### New Features

-

### Enhancements

-

### Libraries

-

### Documentation

-

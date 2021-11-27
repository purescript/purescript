* Implement the IsReflectable type class

  The `IsReflectable` type class is a common interface for reflecting
  type-level values down to the term-level. Its instances are
  automatically solved by the compiler, and it allows `Symbol` and `Int`
  kinded types to be reflected to their term-level representations.

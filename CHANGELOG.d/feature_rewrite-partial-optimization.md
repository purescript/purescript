* Rewrite `Partial` optimization to be cleaner

  This feature shrinks the generated JS code for declarations that use
  empty type classes, such as `Partial`, but is otherwise not expected to
  have user-visible consequences.

* Move the closed record update optimization

  For consumers of CoreFn like alternate backends, the optimization of
  replacing a closed record update with an object literal has now been moved to
  the point of desugaring CoreFn into JS. The `ObjectUpdate` expression
  constructor now contains a `Maybe` field holding a list of record labels to
  be copied as-is, for backends that want to perform this optimization also.

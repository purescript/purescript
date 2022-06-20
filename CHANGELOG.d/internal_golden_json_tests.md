* Compare json files through `aeson` in tests

  This fixes the tests for the graph and source map outputs, as the
  ordering is inconsistent between `stack test` and `cabal test`.

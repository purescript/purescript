* Apply precedence rules to operator sections

  Previously, `(_ * 4 + 1)` would desugar to `\x -> x * (4 + 1)`, even
  though `*` has higher precedence than `+`. Conversely, `(3 * 2 + _)`
  would not compile, even though `*` has higher precedence than `+`. These
  bugs have now been fixed; `(_ * 4 + 1)` is an error, and `(3 * 2 + _)`
  desugars to `\x -> 3 * 2 + x`.

  If you have code that relied on the old behavior, add an extra pair of
  parentheses around the expression in the section.

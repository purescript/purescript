* Float compiler-synthesized function applications

  This is a limited implementation of common subexpression elimination for
  expressions created by the compiler in the process of creating and using
  typeclass dictionaries. Users can expect code that heavily uses typeclasses
  to produce JavaScript that is shorter, simpler, and faster.

  Common subexpression elimination is not applied to any expressions explicitly
  written by users. If you want those floated to a higher scope, you have to do
  so manually.

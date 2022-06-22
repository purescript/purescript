* Add qualification for locally-bound names

  This change makes it so that `Qualified` names can now be qualified by either
  a `ModuleName` for module-level declarations or a `SourceSpan` for bindings
  introduced locally. This makes disambiguation between references to local
  bindings much easier in AST-driven analysis.

  A new desugaring step, `desugarLocals`, has also been added to insert
  `SourceSpan`s into references of local bindings in the AST.
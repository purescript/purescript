* Improve "Unknown value bind" and "Unknown value discard" errors

  The previous error implies that do-notation compiles down to only `bind` or to
  only `discard` (depending on whether the symbol not found was `bind` or
  `discard` respectively), which is somewhat misleading, especially in the
  latter case. Now, the error states correctly that do-notation compiles down to
  both functions.

* Fix two space leaks while compiling many modules

  The first would interleave compilation of too many modules at once, which
  would increase memory usage, especially for single threaded builds with
  `+RTS -N1 -RTS`. Now the number of concurrent modules is limited to
  the number of threads available to the
  [GHC runtime system](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-concurrent.html#rts-options-for-smp-parallelism).

  The second would hold on to memory from modules that compiled with warnings
  until the end of the build when the warnings were printed and the memory freed.
  This is now fixed with additional `NFData` instances.

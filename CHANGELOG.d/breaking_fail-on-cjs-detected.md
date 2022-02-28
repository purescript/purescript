* Fail if CJS is detected in FFI

  Previously, one could write CommonJS in FFI declarations. With the move to ES modules, this leads to a failing compile now.

* Do not emit an error if language-javascript parsing fails

  Previously, an error was emitted when language-javascript parsing failed. With this change, no error or warning is emitted if the js FFI parsing fails.

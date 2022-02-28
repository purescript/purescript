* If FFI parsing succeeds & CommonJS is detected, fail; otherwise, do not error or warn

  Previously, the compiler would emit an error if it failed to parse the FFI JavaScript file.
  Since the underlying JavaScript parser (i.e. `language-javascript`) fails to parse even
  valid JavaScript files, we cannot consider every failed parse to mean invalid JS files.
  Fixing the parser would require a lot of effort, so we are planning to remove it instead
  in `v0.16.x`.
  
  If the parse succeeds and a CommonJS module is detected, a compiler error is now emitted.
  If the parse fails, we no longer emit a compiler error. While we could emit a warning,
  such a warning will quickly become annoying for FFI files that trigger the buggy paths
  of `language-javascript`. Moreover, we presume that all will be migrating their code to
  ES modules now that CommonJS is being deprecated in the larger JavaScript ecosystem.

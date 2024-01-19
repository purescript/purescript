* Add support for `purs compile --source-globs path/to/file`

  Due to a shell character limitation on Windows where a large list of 
  source globs cannot be passed (e.g. `purs compile ... glob1000/src/**/*.purs`),
  source globs can be stored in a file according to the format below
  and the file is passed in instead via `purs compile --source-globs path/to/file`.
  
  ```
  # Lines starting with '#' are comments
  # Blank lines are ignored
  # Otherwise, Every line is a glob

  .spago/foo-1.2.3/src/**/*.purs
  .spago/bar-2.3.3/src/**/*.purs
  my-package/src/**/*.purs
  my-package/tests/**/*.purs
  ```

  `--source-globs` is an optional argument. Mixing it with the normal source globs is fine.
  Assuming `.spago/source-globs` contains `src/**/*.purs`, each command below will use
  the same input globs:
  ```sh
  purs compile src/**/*.purs
  purs compile --source-globs .spago/source-globs
  purs compile src/**/*.purs --source-globs .spago/source-globs
  ```
  
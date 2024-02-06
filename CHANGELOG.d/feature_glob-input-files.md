* Enable passing source input globs via `--source-globs-file path/to/file`

  `--source-globs-file` support has been added to the following commands:
  `compile`, `docs`, `graph`, `ide`, and `publish`.

  Due to a [shell character limitation on Windows](https://learn.microsoft.com/en-us/troubleshoot/windows-client/shell-experience/command-line-string-limitation) where a large list of 
  source globs cannot be passed (e.g. `purs compile ... glob1000/src/**/*.purs`),
  source globs can be stored in a file according to the format below
  and the file is passed in instead via `purs compile ---source-globs-file path/to/file`.
  
  ```
  # Lines starting with '#' are comments.
  # Blank lines are ignored.
  # Otherwise, every line is a glob.

  .spago/foo-1.2.3/src/**/*.purs
  .spago/bar-2.3.3/src/**/*.purs
  my-package/src/**/*.purs
  my-package/tests/**/*.purs
  ```

  `--source-globs-file` is an optional argument. Mixing it with the normal source globs is fine.
  Assuming `.spago/source-globs` contains `src/**/*.purs`, each command below will use
  the same input globs:
  ```sh
  purs compile src/**/*.purs
  purs compile --source-globs .spago/source-globs
  purs compile --source-globs .spago/source-globs src/**/*.purs 
  ```

  In the command...
  ```
  purs compile inputGlob1 inputGlob2 --source-globs-file inputGlobsFoundInFile --exclude-files excludeGlob1
  ``````
  the files passed to the compiler are: all the files found by 
  `inputGlob1`, `inputGlob2`, and `inputGlobsFoundinFile`
  minus the files found by `excludeGlob1`.
  
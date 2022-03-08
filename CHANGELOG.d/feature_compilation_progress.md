* Print compilation progress on the command line

  This feature makes it so `purs compile` and `purs docs` now show
  compilation progress on the command line. Example output:

  ```purs
  [ 1 of 59] Compiling Type.Proxy
  [ 2 of 59] Compiling Type.Data.RowList
  ...
  [58 of 59] Compiling Effect.Class.Console
  [59 of 59] Compiling Test.Main
  ```

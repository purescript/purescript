* Add `--exclude-file` to more commands

  This CLI arg was added to the `compile` command, but not to other commands
  where such a usage would be relevant (e.g. `docs`, `repl`, `graph`, and `ide`).
  
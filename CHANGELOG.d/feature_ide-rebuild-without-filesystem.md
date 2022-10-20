* Allow IDE module rebuilds eschewing the filesystem

  This allows IDE clients to typecheck the module the user is currently typing in without modifying the output.
  This allows for faster feedback cycles in editors and avoids producing a broken `/output` before the user actually saves the file.

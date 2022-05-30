* Add support for optional shebang lines

  One or more shebang line are only allowed as the first lines of a file

  ```purs
  #! a shebang line
  #! another shebang line
  -- | module doc comment
  -- other comment
  module MyModule where

  #! Using a shebang here will fail to parse
  foo :: String
  foo = ""
  ```
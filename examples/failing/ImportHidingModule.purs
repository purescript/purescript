-- @shouldFailWith ImportHidingModule
module A where
  x = 1

module B (module B, module A) where
  import A
  y = 1

module C where
  import B hiding (module A)

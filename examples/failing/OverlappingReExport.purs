-- @shouldFailWith DuplicateValueExport
module A where
  x = true

module B where
  x = false

module C (module A, module M2) where
  import A
  import B as M2

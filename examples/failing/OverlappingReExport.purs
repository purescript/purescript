-- @shouldFailWith DuplicateValueExport
module A where
  x = true

module B where
  x = false

module C (module A, module M2) where
  import A
  import qualified B as M2

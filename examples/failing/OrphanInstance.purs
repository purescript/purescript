-- @shouldFailWith OrphanInstance
module Class where

  class C a where
    op :: a -> a

module Test where

  import Class

  instance cBoolean :: C Boolean where
    op a = a

-- @shouldFailWith OrphanInstance
module Test where

import Class

instance cBoolean :: C Boolean where
  op a = a

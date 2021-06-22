-- @shouldFailWith OrphanInstance
module Test where

import Class

instance C Boolean where
  op a = a

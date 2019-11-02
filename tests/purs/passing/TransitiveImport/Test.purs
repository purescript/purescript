module Test where

import Prelude

class TestCls a where
  test :: a -> a

instance unitTestCls :: TestCls Unit where
  test _ = unit

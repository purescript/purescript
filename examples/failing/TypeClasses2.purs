module TC1 where

class Show a where
  show :: a -> String

module TC2 where

instance TC1.Show String where
  show s = s

module TC3 where

import TC1

test = show "testing"

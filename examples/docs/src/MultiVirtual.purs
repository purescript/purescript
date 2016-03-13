module MultiVirtual
  ( module X )
  where

import MultiVirtual1 as X
import MultiVirtual2 as X


module MultiVirtual1 where

foo :: Int
foo = 1

module MultiVirtual2 
  ( module MultiVirtual2
  , module MultiVirtual3
  ) where

import MultiVirtual3

bar :: Int
bar = 2

module MultiVirtual3 where

baz :: Int
baz = 3

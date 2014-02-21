module RuntimeScopeIssue where

import Prelude

class A a where
  a :: a -> Boolean

class B a where
  b :: a -> Boolean

instance A Number where
  a 0 = true
  a n = b (n - 1)

instance B Number where
  b 0 = false
  b n = a (n - 1)

module Main where

import RuntimeScopeIssue

import Prelude

main = Debug.Trace.print $ a 10

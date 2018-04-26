-- Tests that instances for non-exported classes / types do not appear in the
-- result of `exportedDeclarations`.
module Main
  ( Const(..)
  , class Foo
  , foo
  , main
  ) where

import Prelude
import Effect.Console (log)

data Const a b = Const a

class Foo a where
  foo :: a

data NonexportedType = NonexportedType

class NonexportedClass a where
  notExported :: a

-- There are three places that a nonexported type or type class can occur,
-- leading an instance to count as non-exported:
--  * The instance types
--  * Constraints
--  * The type class itself

-- Case 1: instance types
instance constFoo :: Foo (Const NonexportedType b) where
  foo = Const NonexportedType
else
-- Case 2: constraints
instance nonExportedFoo :: (Foo NonexportedType) => Foo (a -> a) where
  foo = identity
else
-- Another instance of case 2:
instance nonExportedFoo2 :: (NonexportedClass a) => Foo a where
  foo = notExported

-- Case 3: type class
instance nonExportedNonexportedType :: NonexportedClass (Const Int a) where
  notExported = Const 0

main = log "Done"

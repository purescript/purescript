
-- Tests that instances for non-exported classes / types do not appear in the
-- result of `exportedDeclarations`.

module ExportedInstanceDeclarations
  ( Const(..)
  , Foo
  , foo
  ) where

import Prelude

data Const a b = Const a

class Foo a where
  foo :: a

data NonexportedType = NonexportedType

class NonexportedClass a where
  notExported :: a

-- There are three places that a nonexported type or type class can occur,
-- leading an instance to count as non-exported:
--  * Constraints
--  * The type class itself
--  * The instance types

-- Case 1: constraints
instance nonExportedFoo :: (NonexportedClass a) => Foo a where
  foo = notExported

-- Another instance of case 1:
instance nonExportedFoo2 :: (Foo NonexportedType) => Foo (a -> a) where
  foo = id

-- Case 2: type class
instance nonExportedNonexportedType :: NonexportedClass (Const Number a) where
  notExported = Const 0

-- Case 3: instance types
instance constFoo :: Foo (Const NonexportedType b) where
  foo = Const NonexportedType


-- Tests that instances for non-exported classes / types do not appear in the
-- output of psc-docs.


module ClassNotExported
  ( ExportedType(..)
  ) where

class NonexportedClass a where
  m :: a

data ExportedType
  = ExportedType

instance x :: NonexportedClass ExportedType where
  m = ExportedType




module TypeNotExported
  ( ExportedClass
  , m
  ) where

class ExportedClass a where
  m :: a

data NonexportedType
  = NonexportedType

instance x :: ExportedClass NonexportedType where
  m = NonexportedType



-- None of the instances in here should be exported.
module MoreComplexExamples
  ( Const(..)
  , Foo
  , foo
  ) where

data MyUnit = MyUnit
data Const a b = Const a

class Foo a where
  foo :: a

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

-- Case 2: type class
instance nonExportedMyUnit :: NonexportedClass MyUnit where
  notExported = MyUnit

-- Case 3: instance types
instance constFoo :: Foo (Const MyUnit b) where
  foo = Const MyUnit

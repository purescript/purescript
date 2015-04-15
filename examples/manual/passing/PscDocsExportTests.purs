
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


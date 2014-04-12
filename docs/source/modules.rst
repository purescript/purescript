Modules
=======

All code in PureScript is contained in a module. Modules are introduced using the ``module`` keyword::

  module A where
  
  id x = x

When referencing values or data types in another module, names may be qualified by using a dot::

  module B where
  
  foo = A.id

Importing Modules
-----------------

A module can be imported using the ``import`` keyword. This will create aliases for all of the values and types in the imported module::

  module B where
  
  import A

Alternatively, a list of names to import can be provided in parentheses::

  module B where
  
  import A (runFoo)

Values, type constructors and data constructors can all be explicitly imported. A type constructor should be followed by a list of associated data constructors to import in parentheses. A double dot (``..``) can be used to import all data constructors for a given type constructor::

  module B where

  import A (runFoo, Foo(..), Bar(Bar))
  
Modules can also be imported `qualified`, which means that their names will not be brought directly into scope, but rather, aliased to a different module name. This can be helpful when avoiding naming conflicts::

  module Main where
  
  import qualified Data.Array as A
  
  null = ...
  
Here, the name ``null`` would ordinarily conflict with ``null`` from ``Data.Array``, but the qualified import solves this problem. ``Data.Array.null`` can be referenced using ``A.null`` instead.

Module Exports
--------------

Module exports can be restricted to a set of names by providing that set in parentheses in the module declaration::

  module A (runFoo, Foo(..)) where

The types of names which can be exported is the same as for module imports.

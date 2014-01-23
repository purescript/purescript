Modules
=======

All code in PureScript is contained in a module. Modules are introduced using the `module` keyword::

  module A where
  
  id x = x

When referencing values or data types in another module, names may be qualified by using a dot::

  module B where
  
  foo = A.id

Importing Modules
-----------------

A module can be imported using the `import` keyword. This will create aliases for all of the values and types in the imported module::

  module B where
  
  import A

Alternatively, a list of names to import can be provided in parentheses::

  module B where
  
  import A (id)

Type Classes
============

PureScript has basic support for type classes via the ``class`` and ``instance`` keywords. 

Types appearing in class instances are must be of the form ``String``, ``Number``, ``Boolean``, or ``C t1 ... tn`` where ``C`` is a type constructor (including ``[]`` and ``->`` and ``t_i`` are types of the same form).

Type class instances are resolved based on the order in which they appeared in the source files. In particular, overlapping instances are permitted.

Here is an example of the ``Show`` typeclass, with instances for ``String``, ``Booleans`` and ``[]``::

  class Show a where
    show :: a -> String
  
  instance showString :: Show String where
    show s = s
  
  instance showBoolean :: Show Boolean where
    show true = "true"
    show false = "false"
  
  instance showArray :: (Show a) => Show [a] where
    show [] = "[]"
    show (x:xs) = show x ++ " : " ++ showArray xs
  
  example = show [true, false]

Type Classes
============

PureScript has basic support for type classes via the ``class`` and ``instance`` keywords. 

Types appearing in class instances are must be of the form ``String``, ``Number``, ``Boolean``, or ``C t1 ... tn`` where ``C`` is a type constructor (including ``[]`` and ``->`` and ``t_i`` are types of the same form).

Type class instances are resolved based on the order in which they appeared in the source files. Overlapping instances will result in a compilation error.

Example
-------

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

Superclasses
------------

Superclass implications can be indicated in a class declaration with a backwards fat arrow ``<=``::

  class (Monad m) <= MonadFail m where
    fail :: forall a. String -> m a
    
Superclass instances will be used when searching for an instance of a subclass. For example, in the code below, the ``Monad`` constraint introduced by the ``return`` function can be discharged since ``Monad`` is a superclass of ``MonadFail``::

  assert :: forall m. (MonadFail m) => Bool -> m {}
  assert true = return {}
  assert false = fail "Assertion failed"
  
Type Annotations
----------------

A constrained type will never be inferred for a top-level declaration without an explicit type signature. 

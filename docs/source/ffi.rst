Foreign Function Interface
==========================

Importing Values
----------------

The ``foreign import`` keywords declare a value which is defined in Javascript, and its type::

  foreign import pow :: Number -> Number -> Number

Inline Javascript
-----------------

A foreign import declaration may optionally contain its definition in Javascript as a string literal. If this is provided, the string will be inserted directly into the generated Javascript before the current module definition::

  foreign import pow 
    "function pow(n) {\
    \  return function(p) {\
    \    return Math.pow(n, p);\
    \  };\
    \}" :: Number -> Number -> Number

Importing Types
---------------

To declare a new type with no constructors, use ``foreign import data`` and provide the kind::

  foreign import data DOM :: *
  	
  foreign import document :: { 
    createElement :: String -> DOM  
  }

Importing Type Class Instances
------------------------------

Type class instances can be imported with ``foreign import instance``::

  foreign import instance showString :: Prelude.Show String


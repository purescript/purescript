Prelude
=======

The Prelude is a collection of standard modules which define types for standard Javascript functions, as well as some common functions that will be familiar to users of Haskell or similar functional languages.

Prelude Module
--------------

===========  ============================================================  ===========================================
Name         Type                                                          Description
===========  ============================================================  ===========================================
``id``       ``forall a. a -> a``                                          Identity function
``flip``     ``forall a b c. (a -> b -> c) -> b -> a -> c``                Flips the order of a function's arguments
``konst``    ``forall a b. a -> b -> a``                                   Defines a constant function
``(<|)``     ``forall a b c. (a -> b) -> (b -> c) -> a -> c``              Function composition
``(|>)``     ``forall a b c. (b -> c) -> (a -> b) -> a -> c``              Function composition
``($)``      ``forall a b. (a -> b) -> a -> b``                            Function application
===========  ============================================================  ===========================================

=======  ===========================================
Name     Description
=======  ===========================================
Show     Type class for types whose values can be
         converted to strings
Monad    Type class for type constructors which
         support ``do`` notation
=======  ===========================================

String Module
-------------

`TODO`

Regex Module
------------

`TODO`

Maybe Module
------------

`TODO`

Either Module
-------------

`TODO`
  
Arrays Module
-------------

`TODO`
  
Tuple Module
------------

`TODO`
  
Eff Module
----------

`TODO`
  
Errors Module
-------------

`TODO`
  
IORef Module
------------

`TODO`
  
Random Module
-------------

`TODO`
  
ST Module
---------

`TODO`
  
Trace Module
------------

`TODO`
 
Kind System
===========

The kind system defines the following kinds:

- ``*``, the kind of types.
- ``!``, the kind of effects.
- Arrow kinds ``k1 -> k2``
- Row kinds ``# k``

Row Kinds
---------

The kind ``# k`` of rows is used to classify labelled, unordered collections of types of kind ``k``. 

For example ``# *`` is the kind of rows of types, as used to define records, and ``# !`` is the kind of rows of effects, used to define the monad ``Eff`` of extensible effects.

Quantification
--------------

A type variable can refer to not only a type or a row, but a type constructor, or row constructor etc., and type variables with those kinds can be bound inside a ``forall`` quantifier.

Types
=====

The type system defines the following types:

- Primitive Types: Number, String, Boolean
- Arrays 
- Records
- Tagged Unions
- Functions
- Polymorphic Types
- Constrained Types
- Type Synonyms

Primitive Types
---------------

The three primitive types ``String``, ``Number`` and ``Boolean`` correspond to their Javascript equivalents at runtime.

Arrays
------

PureScript arrays correspond to Javascript arrays at runtime, but all elements in an array must have the same type.

Records
-------

PureScript records correspond to Javascript objects. They may have zero or more named fields, each with their own types.

Tagged Unions
-------------

Tagged unions consist of one or more constructors, each of which takes zero or one arguments.

Tagged unions can only be created using their constructors, and deconstructed through pattern matching (see later).

For example::

  data Foo a = Foo | Bar String
  
  runFoo Foo = "It's a Foo"
  runFoo (Bar s) = "It's a Bar. The string is " ++ s
  
  test = runFoo Foo ++ runFoo (Bar "Test")

In the example, Foo is a tagged union type which has two constructors. It's first constructor `Foo` takes no argument, and it's second ``Bar`` takes one, which must be a String.

``runFoo`` is an example of pattern matching on a tagged union type to discover its constructor, and the last line shows how ``Foo``s are constructed.

Functions
---------

Functions in PureScript can have zero or more arguments. A function with multiple arguments is not to be confused with a `curried function`, which takes a single argument, and returns another function.

Polymorphic Types
-----------------

Expressions defined at the top level may have polymorphic types.

Here is an example::

  identity x = x

`identity` is inferred to have (polymorphic) type ``forall t0. t0 -> t0``. This means that for any type ``t0``, ``identity`` can be given a value of type ``t0`` and will give back a value of the same type.

A type annotation can also be provided::

  identity :: forall a. a -> a
  identity x = x

Functions may also be polymorphic in row types or type variables with other kinds (see "Kind System")::

  addProps o = o.foo + o.bar
    
Here, `addProps` is inferred to have type ``forall r. { foo :: Number, bar :: Number | r } -> Number`.` That is, it can take any type which has properties ``Foo`` and ``Bar``, and *any other record properties*.

So, the following compiles::

  addProps { foo: 1, bar: 2, baz: 3 }
    
but the following does not::

  addProps { foo: 1 }
    
since the `bar` property is missing.

Again, a type annotation can be provided if necessary.

Rank N Types
------------

It is also possible for the ``forall`` quantifier to appear on the left of a function arrow, inside types record fields and data constructors, and in type synonyms.

In most cases, a type annotation is necessary when using this feature.

As an example, we can pass a polymorphic function as an argument to another function::

  poly :: (forall a. a -> a) -> Boolean
  poly f = (f 0 < 1) == f true

Notice that the polymorphic function's type argument is instantiated to both `Number` and `Boolean`.

An argument to ``poly`` must indeed be polymorphic. For example, the following fails::

  test = poly (\n -> n + 1)

since the skolemized type variable ``a`` does not unify with ``Number``.

Type Synonyms
-------------

For convenience, it is possible to declare a synonym for a type using the ``type`` keyword. Type synonyms can include type arguments.

For example::

  type Foo = { foo :: Number, bar Number }
  
  addFoo :: Foo -> Number
  addFoo = \o -> o.foo + o.bar
  
Constrained Types
-----------------

Polymorphic types may be predicated on one or more ``constraints``. See the chapter on type classes for more information.

Type Annotations
----------------

Most types can be inferred (not including Rank N Types and constrained types), but annotations can optionally be provided using a double-colon::

  one = 1 :: Number

Kind System
-----------

There are two primitive kinds, the kind ``*`` of types and the kind ``!`` of effects. 

For each kind ``k`` there is also a kind ``# k`` of rows, with types of kind ``k``. For example ``# *`` is the kind of rows of types, as used to define records, and ``# !`` is the kind of rows of effects, used to define the monad ``Eff`` of extensible effects.

Type constructors are given the arrow kind ``k1 -> k2`` for appropriate kinds ``k1``, ``k2``.

A type variable can refer to not only a type or a row, but a type constructor, or row constructor etc., and type variables with those kinds can be bound inside a ``forall`` quantifier.

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
- Rows

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

Tagged unions consist of one or more constructors, each of which takes zero or more arguments.

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

Functions in PureScript are like their Javascript counterparts, but always have exactly one argument.

Polymorphic Types
-----------------

Expressions defined at the top level may have polymorphic types.

Here is an example::

  identity x = x

`identity` is inferred to have (polymorphic) type ``forall t0. t0 -> t0``. This means that for any type ``t0``, ``identity`` can be given a value of type ``t0`` and will give back a value of the same type.

A type annotation can also be provided::

  identity :: forall a. a -> a
  identity x = x

Row Polymorphism
----------------

Polymorphism is not limited to abstracting over types. Values may also be polymorphic in types with other kinds, such as rows or effects (see "Kind System").

For example, the following function accesses two properties on a record::

  addProps o = o.foo + o.bar
    
The inferred type of `addProps` is

::
  forall r. { foo :: Number, bar :: Number | r } -> Number
  
Here, the type variable ``r`` has kind ``# *`` - it represents a `row` of `types`. It can be instantiated with any row of named types.

In other words, ``addProps`` accepts any record which has properties ``Foo`` and ``Bar``, and *any other record properties*.

Therefore, the following application compiles::

  addProps { foo: 1, bar: 2, baz: 3 }
    
even though the type of ``addProps`` does not mention the property ``baz``. However, the following does not compile::

  addProps { foo: 1 }
    
since the `bar` property is missing.

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

Rows
----

A row of types represents an unordered collection of named types without duplicates.

Rows have kind ``# k`` for some kind ``k``, and so values do not have types which are rows. Rather, rows can be used in type signatures to define record types or other type where labelled, unordered types are useful.

To denote a closed row, separate the fields with commas, with each label separated from its type with a double colon::

  (name :: String, age :: Number)
  
It may be necessary, depending on the context, to surround a row in parentheses.

To denote an open row (i.e. one which may unify with another row to add new fields), separate the specified terms from a row variable by a pipe::

  (name :: String, age :: Number | r)

Type Synonyms
-------------

For convenience, it is possible to declare a synonym for a type using the ``type`` keyword. Type synonyms can include type arguments.

For example::

  type Foo = { foo :: Number, bar :: Number }
  
  addFoo :: Foo -> Number
  addFoo = \o -> o.foo + o.bar
  
Constrained Types
-----------------

Polymorphic types may be predicated on one or more ``constraints``. See the chapter on type classes for more information.

Type Annotations
----------------

Most types can be inferred (not including Rank N Types and constrained types), but annotations can optionally be provided using a double-colon::

  one = 1 :: Number

Types
=====

The type system defines the following types:

- Number
- String
- Boolean
- Arrays 
- Records
- Tagged Unions
- Functions
- Polymorphic Types
- Constrained Types
- Type Synonyms

Primitive Types
---------------

The three primitive types `String`, `Number` and `Boolean` correspond to their Javascript equivalents at runtime.

PureScript supports the same binary and unary operations on primitive types as Javascript, with the following exceptions:

- String concatenation is denoted `++` to differentiate it from numeric addition, `+`
- PureScript's `==` and `!=` correspond to Javascript's strong equality tests `===` and `!==`

Examples::

  num = 1 + 2 * 3
  str = "Hello, " ++ "World!"
  bool = 1 > 2 || true

Arrays
------

PureScript arrays correspond to Javascript arrays at runtime, but all elements must have the same type.

Array literals look like Javascript array literals: `[1, 2, 3]`

Array elements can be read using array index notation `arr !! index`

Records
-------

PureScript records correspond to Javascript objects.

Record literals look like Javascript object literals: `{ foo: "Foo" }`

Properties can be read by using dot notation: `o.foo`

Tagged Unions
-------------

Tagged unions consist of one or more constructors, each of which takes zero or one arguments.

Tagged unions can only be created using their constructors, and deconstructed through pattern matching (see later).

For example::

  data Foo a = Foo | Bar String
  
  runFoo Foo = "It's a Foo"
  runFoo (Bar s) = "It's a Bar. The string is " ++ s
  
  test = runFoo Foo ++ runFoo (Bar "Test")

In the example, Foo is a tagged union type which has two constructors. It's first constructor `Foo` takes no argument, and it's second `Bar` takes one, which must be a String.

`runFoo` is an example of pattern matching on a tagged union type to discover its constructor, and the last line shows how `Foo`s are constructed.

Functions
---------

Functions in PureScript can have zero or more arguments in general, just like in Javascript.

Functions are introduced by using a backslash followed by a list of argument names::

  example1 = \a b -> a + b

which would correspond to the following Javascript::

  function example1(a) {
    return function (b) { 
      return a + b;
    }
  }

Multiple argument functions can be introduced by wrapping the arguments in parentheses, and separating them with commas:

  example2 = \(a, b) -> a + b

which generates::

  function example2(a, b) { 
    return a + b;
  }
    
In the case of a function with no arguments, the parentheses may be omitted, as follows::

  example3 = \ -> 100

which would correspond to the Javascript::

  function example3() { 
    return 100; 
  }

Multiple-argument and single-argument syntax can be mixed, as follows::

  example4 = \a (b, c) d -> a + b + c + d

which generates::

  function example4(a) {
      return function (b, c) {
          return function (d) {
              return a + b + c + d;
          }
      }
  }

Functions are applied by providing their arguments inside parentheses::

  test1(1, 2, 3)
  test2()

A special case is made in the case of functions with one argument. These functions can be applied without parentheses, and function application associates to the left::

  -- has type Number -> Number -> Number -> Number
  addThree = \a b c -> a + b + c
  
  -- has type Number -> Number -> Number
  addThree 1 
  
  -- has type Number -> Number
  addThree 1 2 
  
  -- has type Number
  addThree 1 2 3 

Polymorphic Types
-----------------

Expressions defined at the top level may have polymorphic types.

Here is an example::

  identity x = x

`identity` is inferred to have (polymorphic) type `forall t0. t0 -> t0`. This means that for any type `t0`, `identity` can be given a value of type `t0` and will give back a value of the same type.

A type annotation can also be provided::

  identity :: forall a. a -> a
  identity x = x

Functions may also be polymorphic in row types or type variables with other kinds (see "Kind System")::

  addProps o = o.foo + o.bar
    
Here, `addProps` is inferred to have type `forall r. { foo :: Number, bar :: Number | r } -> Number`. That is, it can take any type which has properties `Foo` and `Bar`, and *any other record properties*.

So, the following compiles::

  addProps { foo: 1, bar: 2, baz: 3 }
    
but the following does not::

  addProps { foo: 1 }
    
since the `bar` property is missing.

Again, a type annotation can be provided if necessary.

Rank N Types
------------

It is also possible for the `forall` quantifier to appear on the left of a function arrow, inside types record fields and data constructors, and in type synonyms.

In most cases, a type annotation is necessary when using this feature.

As an example, we can pass a polymorphic function as an argument to another function::

  poly :: (forall a. a -> a) -> Boolean
  poly f = (f 0 < 1) == f true

Notice that the polymorphic function's type argument is instantiated to both `Number` and `Boolean`.

An argument to `poly` must indeed be polymorphic. For example, the following fails::

  test = poly (\n -> n + 1)

since the skolemized type variable `a` does not unify with `Number`.

Type Synonyms
-------------

For convenience, it is possible to declare a synonym for a type using the `type` keyword. Type synonyms can include type arguments.

For example::

  type Foo = { foo :: Number, bar Number }
  
  addFoo :: Foo -> Number
  addFoo = \o -> o.foo + o.bar

Type Annotations
----------------

Most types can be inferred (not including Rank N Types and constrained types), but annotations can optionally be provided using a double-colon::

  one = 1 :: Number

Kind System
-----------

There are two primitive kinds, the kind `*` of types and the kind `!` of effects. 

For each kind `k` there is also a kind `# k` of rows, with types of kind `k`. For example `# *` is the kind of rows of types, as used to define records, and `# !` is the kind of rows of effects, used to define the monad `Eff` of extensible effects.

Type constructors are given the arrow kind `k1 -> k2` for appropriate kinds `k1`, `k2`.

A type variable can refer to not only a type or a row, but a type constructor, or row constructor etc., and type variables with those kinds can be bound inside a `forall` quantifier.

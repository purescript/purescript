Syntax
======

Whitespace Rules
----------------

Syntax is whitespace sensitive. The general rule of thumb is that declarations which span multiple lines should be indented past the column on which they were first defined on their subsequent lines.

That is, the following is valid::

  foo = bar(x) + 
    baz(x)

But this is not::

  foo = bar(x) + 
  baz(x)
  
Operators
---------

Binary operators in PureScript are just regular functions. The Prelude defines a number of operators which correspond to Javascript's operators.

The Prelude defines the following functions which correspond to Javascript's unary operators:

==========  ========  =======================
Function    Operator  Meaning
==========  ========  =======================
negate      \-        Numeric negation
not         !         Boolean negation
complement  ~         Binary negation
==========  ========  =======================

and the following binary operators:

==========  ===========  =====================
Function    JS Operator  Meaning
==========  ===========  =====================
\+          \+           Numeric addition
\-          \-           Numeric subtraction
\*          \*           Numeric multiplication
/           /            Numeric division
%           %            Numeric modulus
==          ==           Equality check
/=          !=           Inequality check
<           <            Less than
<=          <=           Less than or equal
>           >            Greater than
>=          >=           Greater than or equal
&&          &&           Boolean AND
||          ||           Boolean OR
&           &            Binary AND
\|          \|           Binary OR
^           ^            Binary XOR
shl         <<           Shift Left
shr         >>           Shift Right
zshr        \>>>         Zero-Fill Shift Right
++          \+           String concatenation
==========  ===========  =====================

Literal Values
--------------

Numeric literals can be integers or floating point numbers. Numbers in hexadecimal notation should be preceded by the characters ``0x``::

  16
  16.0
  0xF0
  
String literals are enclosed in double-quotes and may extend over multiple lines. Line breaks should be surrounded by slashes as follows::

  "Hello World"
  
  "Hello \
  \World"
  
The two boolean literals are ``true`` and ``false``.

Array Literals
--------------

Array literals are surrounded by square brackets, as in Javascript::

  []
  [1, 2, 3]
  
Record Literals
---------------
  
Record literals are surrounded by braces, as in Javascript::

  {}
  { foo: "Foo", bar: 1 }
  
Array Indexing
--------------

The ``Prelude.Unsafe`` module defines the ``unsafeIndex`` function which retrieves the element of an array at an index::

  unsafeIndex :: forall a. [a] -> Number -> a
  
The code generator will turn the expression ``unsafeIndex arr index`` into the simplified Javascript ``arr[index]``.

The `purescript-array` core library defines an alternative safe version ``!!`` of ``unsafeIndex`` which checks arrays bounds and returns a value of type ``Maybe a``.
  
Property Accessors
------------------

To access a property of a record, use a dot, followed by the property name, as in Javascript::

  rec.propertyName
  
Functions
---------

Functions are introduced by using a backslash followed by a list of argument names::

  example1 = \a b -> a + b

which would correspond to the following Javascript::

  function example1(a) {
    return function (b) { 
      return a + b;
    }
  }

Function application is indicated by just the juxtaposition of a function with its arugments::

  example1 10 20

Functions can also be defined at the top level by providing a list of patterns and an optional guard on the left hand side of the equals sign::

  f [] [] = []
  f (x:xs) (y:ys) = x + y : f xs ys
      
If-Then-Else Expressions
------------------------

The ``if``, ``then`` and ``else`` keywords can be used to create conditional expressions. In this case, the ``else`` block is always required.

For example::

  conditional = if 2 > 1 then "ok" else "oops"

Do Notation
-----------

The ``do`` keyword introduces simple syntactic sugar for monadic expressions.

Here is an example, using the maybe monad::

  data Maybe a = Nothing | Just a
  
  isEven :: Number -> Maybe {}
  isEven n | n % 2 == 0 = Just {}
  isEven _ = Nothing
  
  evenSum a b = do
    n <- a
    m <- b
    let sum = n + m
    isEven sum
    return sum

``isEven`` adds two values of type ``Maybe Number`` and returns their sum, if the sum is even. If the sum is odd, ``evenSum`` returns ``Nothing``.

When using ``do`` notation, the corresponding type constructor must be an instance of the ``Prelude.Monad`` type class.

Statements can have the following form:

- ``a <- x`` which desugars to ``x >>= \a -> ...` `
- ``x`` which desugars to ``x >>= \_ -> ...`` or just ``x`` if this is the last statement.
- A let binding ``let a = x`` where ``a`` can be either a name or a binder. Note the lack of the ``in`` keyword.

Binders can be used on the left hand side of ``<-`` or ``=``. For example::

  test arr = do
    (x:y:_) <- arr
    ret (x + y)

A pattern match failure will generate a runtime exception, just as in the case of a regular ``case`` statement.

Operators
---------

User-defined infix operators can be created by enclosing names in parentheses.

E.g. to create a synonym for string concatenation::

  (<>) = \s1 s2 -> s1 ++ s2
  
  greeting = "Hello" <> "World!"

Regular functions can be used as operators by enclosing their names in backticks::

  foo = \x y -> x * y + y
  
  test = 10 `foo` 20
    
Fixity declarations can associate a precedence level, which is a natural number, to a user-defined operator, and specify which way it associates::

  infixl 5 <>
  infixr 7 %%

Record Updates
--------------

Properties on records can be updated using the following syntax:: 

  o { key = value, ..., key = value }

For example, the following function increments the ``foo`` property on its argument::

  incr o = o { foo = o.foo + 1 }

Let Bindings
------------

The ``let`` keywords a collection of local declarations, which may be mutually recursive, and which may include type declarations::

  factorial :: Number -> Number
  factorial = 
    let
      go :: Number -> Number -> Number
      go acc 1 = acc
      go acc n = go (acc * n) (n - 1)
    in
      go 1

The ``where`` keyword can also be used to introduce local declarations `at the end of a value declaration`::

  factorial :: Number -> Number
  factorial = go 1
    where
    go :: Number -> Number -> Number
    go acc 1 = acc
    go acc n = go (acc * n) (n - 1)


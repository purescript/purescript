## Hello, PureScript!

As an introductory example, here is the usual "Hello World" written in PureScript:

```purescript
module Main where
  
import Control.Monad.Eff.Console
  
main = log "Hello, World!"
```

## Another Example

The following code defines a `Person` data type and a function to generate a string representation for a `Person`:

```purescript
data Person = Person { name :: String, age :: Number }
  
showPerson :: Person -> String
showPerson (Person o) = o.name ++ ", aged " ++ show o.age
  
examplePerson :: Person
examplePerson = Person { name: "Bonnie", age: 26 }
```

Line by line, this reads as follows:

- ``Person`` is a data type with one constructor, also called ``Person``
- The ``Person`` constructor takes an object with two properties, ``name`` which is a ``String``, and ``age`` which is a ``Number``
- The ``showPerson`` function takes a ``Person`` and returns a ``String``
- ``showPerson`` works by case analysis on its argument, first matching the constructor ``Person`` and then using string concatenation and object accessors to return its result.
- ``examplePerson`` is a Person object, made with the ``Person`` constructor and given the String "Bonnie" for the name value and the Number 26 for the age value.

# Types

The type system defines the following types:

- Primitive Types: Number, String, Boolean
- Integers
- Arrays 
- Records
- Tagged Unions
- Newtypes
- Functions
- Polymorphic Types
- Constrained Types
- Type Synonyms
- Rows

## Primitive Types

The primitive types ``String``, ``Number`` and ``Boolean`` correspond to their Javascript equivalents at runtime.

## Integers

The `Int` type represents integer values. The runtime representation is also a normal JavaScript Number; however, operations like `(+)` on `Int` values are defined differently in order to ensure that you always get `Int` values as a result.

## Arrays

PureScript arrays correspond to Javascript arrays at runtime, but all elements in an array must have the same type.

## Records

PureScript records correspond to Javascript objects. They may have zero or more named fields, each with their own types.

## Tagged Unions

Tagged unions consist of one or more constructors, each of which takes zero or more arguments.

Tagged unions can only be created using their constructors, and deconstructed through pattern matching (see later).

For example::

```purescript
data Foo = Foo | Bar String
  
runFoo Foo = "It's a Foo"
runFoo (Bar s) = "It's a Bar. The string is " ++ s
  
test = runFoo Foo ++ runFoo (Bar "Test")
```

In the example, Foo is a tagged union type which has two constructors. Its first constructor ``Foo`` takes no arguments, and its second ``Bar`` takes one, which must be a String.

``runFoo`` is an example of pattern matching on a tagged union type to discover its constructor, and the last line shows how to construct values of type ``Foo``.

## Newtypes

Newtypes are like data types (which are introduced with the ``data`` keyword), but are restricted to a single constructor which contains a single argument. Newtypes are introduced with the ``newtype`` keyword::

```purescript
newtype Percentage = Percentage Number
```

The representation of a newtype at runtime is the same as the underlying data type. For example, a value of type ``Percentage`` is just a JavaScript number at runtime.
  
Newtypes can be assigned their own type class instances, so for example, ``Percentage`` can be given its own ``Show`` instance::

  instance showPercentage :: Show Percentage where
    show (Percentage n) = show n ++ "%"

## Functions

Functions in PureScript are like their Javascript counterparts, but always have exactly one argument.

## Polymorphic Types

Expressions can have polymorphic types:

```purescript
identity x = x
```

``identity`` is inferred to have (polymorphic) type ``forall t0. t0 -> t0``. This means that for any type ``t0``, ``identity`` can be given a value of type ``t0`` and will give back a value of the same type.

A type annotation can also be provided::

```purescript
identity :: forall a. a -> a
identity x = x
```

## Row Polymorphism

Polymorphism is not limited to abstracting over types. Values may also be polymorphic in types with other kinds, such as rows or effects (see "Kind System").

For example, the following function accesses two properties on a record::

```purescript
addProps o = o.foo + o.bar + 1
```
    
The inferred type of ``addProps`` is::

```purescript
forall r. { foo :: Int, bar :: Int | r } -> Number
```
  
Here, the type variable ``r`` has kind ``# *`` - it represents a `row` of `types`. It can be instantiated with any row of named types.

In other words, ``addProps`` accepts any record which has properties ``foo`` and ``bar``, and *any other record properties*.

Therefore, the following application compiles::

```purescript
addProps { foo: 1, bar: 2, baz: 3 }
```
    
even though the type of ``addProps`` does not mention the property ``baz``. However, the following does not compile::

```purescript
addProps { foo: 1 }
```
    
since the ``bar`` property is missing.

## Rank N Types

It is also possible for the ``forall`` quantifier to appear on the left of a function arrow, inside types record fields and data constructors, and in type synonyms.

In most cases, a type annotation is necessary when using this feature.

As an example, we can pass a polymorphic function as an argument to another function::

```purescript
poly :: (forall a. a -> a) -> Boolean
poly f = (f 0 < 1) == f true
```

Notice that the polymorphic function's type argument is instantiated to both ``Number`` and ``Boolean``.

An argument to ``poly`` must indeed be polymorphic. For example, the following fails::

```purescript
test = poly (\n -> n + 1)
```

since the skolemized type variable ``a`` does not unify with ``Int``.

## Rows

A row of types represents an unordered collection of named types without duplicates.

Rows have kind ``# k`` for some kind ``k``, and so values do not have types which are rows. Rather, rows can be used in type signatures to define record types or other type where labelled, unordered types are useful.

To denote a closed row, separate the fields with commas, with each label separated from its type with a double colon::

```purescript
(name :: String, age :: Number)
```

It may be necessary, depending on the context, to surround a row in parentheses. (However, see #

To denote an open row (i.e. one which may unify with another row to add new fields), separate the specified terms from a row variable by a pipe::

```purescript
(name :: String, age :: Number | r)
```

## Type Synonyms

For convenience, it is possible to declare a synonym for a type using the ``type`` keyword. Type synonyms can include type arguments.

For example::

```purescript
type Foo = { foo :: Number, bar :: Number }
  
addFoo :: Foo -> Number
addFoo o = o.foo + o.bar

type Bar a = { foo :: a, bar :: a }

combineBar :: forall a b. (a -> a -> b) -> Bar a -> b
combineBar f o = f o.foo o.bar
```
  
## Constrained Types

Polymorphic types may be predicated on one or more ``constraints``. See the chapter on type classes for more information.

## Type Annotations

Most types can be inferred (not including Rank N Types and constrained types), but annotations can optionally be provided using a double-colon::

```purescript
one = 1 :: Int

-- Either of the following will type check, because the first is a more general form of the second
x = one :: forall a. (Semiring a) => a
x = one :: Int
```

## Kind System

The kind system defines the following kinds:

- ``*``, the kind of types.
- ``!``, the kind of effects.
- Arrow kinds ``k1 -> k2``
- Row kinds ``# k``

## Row Kinds

The kind ``# k`` of rows is used to classify labelled, unordered collections of types of kind ``k``. 

For example ``# *`` is the kind of rows of types, as used to define records, and ``# !`` is the kind of rows of effects, used to define the monad ``Eff`` of extensible effects.

## Quantification

A type variable can refer to not only a type or a row, but a type constructor, or row constructor etc., and type variables with those kinds can be bound inside a ``forall`` quantifier.

# Syntax

## Whitespace Rules

Syntax is whitespace sensitive. The general rule of thumb is that declarations which span multiple lines should be indented past the column on which they were first defined on their subsequent lines.

That is, the following is valid:

``` purescript
foo = bar +
  baz
```

But this is not:

``` purescript
foo = bar +
baz
```

## Comments

A single line comment starts with `--`:

``` purescript
-- This is a comment
```

Multi-line comments are enclosed in `{-` and `-}`. These can be nested:

``` purescript
{-
  Comment
  {- nested comment -}
  continued comment
-}
```

## Top-level declarations

Values at the top level of a module are defined by providing a name followed by an equals sign and then the value to associate:

``` purescript
one = 1
```

Functions can also be defined at the top level by providing a list of patterns on the left hand side of the equals sign:

``` purescript
add x y = x + y
```

See the section on pattern matching for more details about the kinds of patterns that can be used here.

Functions using pattern matching may be defined multiple times to handle different pattern matches:

``` purescript
isEmpty [] = true
isEmpty _ = false
```

This does not mean functions can be arbitrarily overloaded with different numbers or types of arguments though.

Guards can also be used in these definitions:

``` purescript
isEmptyAlt xs | length xs == 0 = true
isEmptyAlt _ = false
```

A top level declaration is generally defined with a type signature:

```purescript
multiply :: Number -> Number -> Number
multiply x y = x * y
```

Type signatures are not required for top-level declarations in general, but is good practice to do so. See the section on types for more details.

## Function application

Function application is indicated by just the juxtaposition of a function with its arguments:

``` purescript
add 10 20
```

PureScript functions are defined as curried, so partial application has no special syntax:

``` purescript
add10 = add 10
```

# Values

## Numbers

Numeric literals can be integers (type `Int`) or floating point numbers (type `Number`). Floating point numbers are identified by a decimal point. Integers in hexadecimal notation should be preceded by the characters `0x`:

``` purescript
16 :: Int
0xF0 :: Int
16.0 :: Float
```

## Strings

String literals are enclosed in double-quotes and may extend over multiple lines. Line breaks should be surrounded by slashes as follows:

``` purescript
"Hello World"

"Hello \
\World"
```

Line breaks will be omitted from the string when written this way. If line breaks are required in the output, they can be inserted with `\n`, or by using an alternate string syntax, where the string is enclosed in triple double-quotes. This also allows the use of double quotes within the string with no need to escape them:

``` purescript
jsIsHello :: String
jsIsHello = """
function isHello(greeting) {
  return greeting === "Hello";
}
"""
```

## Booleans

The boolean literals are `true` and `false`.

## Functions

Function values (sometimes called _lambdas_) are introduced by using a backslash followed by a list of argument names:

``` purescript
\a b -> a + b
```

which would correspond to the following JavaScript:

``` javascript
function (a) {
  return function (b) {
    return a + b;
  }
}
```

## Arrays

Array literals are surrounded by square brackets, as in JavaScript:

``` purescript
[]
[1, 2, 3]
```

## Records

Record literals are surrounded by braces, as in JavaScript:

``` purescript
{}
{ foo: "Foo", bar: 1 }
```

Record literals with wildcards can be used to create a function that produces the record instead:

``` purescript
{ foo: _, bar: _ }
```

is equivalent to:

``` purescript
\foo bar -> { foo: foo, bar: bar }
```

## Property Accessors

To access a property of a record, use a dot followed by the property name, as in JavaScript:

``` purescript
rec.propertyName
```

There are also partially applied accessors, where an underscore is followed by a property name:

``` purescript
_.propertyName
```

This is equivalent to:

``` purescript
\rec -> rec.propertyName
```

## Record Updates

Properties on records can be updated using the following syntax:

``` purescript
rec { key1 = value1, ..., keyN = valueN }
```

For example, the following function increments the `foo` property on its argument:

``` purescript
\rec -> rec { foo = rec.foo + 1 }
```

Wildcards can also be used in updates to produce a partially applied update:

``` purescript
rec { foo = _ }
```

This is equivalent to:

``` purescript
\foo -> rec { foo = foo }
```

An underscore can also appear in the object position in an updater:

``` purescript
_ { foo = 1 }
```

This is equivalent to:

``` purescript
\rec -> rec { foo = 1 }
```

## Operators

Operators in PureScript are just regular functions. The [`Prelude`](https://github.com/purescript/purescript/tree/master/prelude) defines a number of operators which correspond to JavaScript operators.

## Unary operators

Function     | JS Operator | Meaning
------------ | ----------- | ----------------
`negate`     |  `-`        | Numeric negation
`not`        |  `!`        | Boolean negation
`complement` |  `~`        | Binary negation

## Binary operators

Function   | JS Operator | Meaning
---------- | ----------- | ----------------------
`+`        | `+`         | Numeric addition
`-`        | `-`         | Numeric subtraction
`*`        | `*`         | Numeric multiplication
`/`        | `/`         | Numeric division
`%`        | `%`         | Numeric modulus
`==`       | `==`        | Equality check
`/=`       | `!=`        | Inequality check
`<`        | `<`         | Less than
`<=`       | `<=`        | Less than or equal
`>`        | `>`         | Greater than
`>=`       | `>=`        | Greater than or equal
`&&`       | `&&`        | Boolean AND
`||`       | `||`        | Boolean OR
`&`        | `&`         | Binary AND
`|`        | `|`         | Binary OR
`^`        | `^`         | Binary XOR
`shl`      | `<<`        | Shift Left
`shr`      | `>>`        | Shift Right
`zshr`     | `>>>`       | Zero-fill Shift Right
`++`       | `+`         | String concatenation*

Many of these operators are defined in type classes and work with lots of different types. For example, `+` and `*` work with not only `Int` or `Number`, but any `Semiring` (see "Type classes").

## Operators as values

Operators can be used as normal values by surrounding them with parentheses:

``` purescript
and = (&&)
```

## Operator sections

Binary operators can be partially applied by listing them with one operand surrounded by parentheses:

``` purescript
half = (/ 2)
double = (2 *)
```

## Functions as operators

Functions can be used as infix operators when they are surrounded by backticks:

``` purescript
foo x y = x * y + y
test = 10 `foo` 20
```

Operator sections also work for functions used this way:

``` purescript
fooBy2 = (`foo` 2)
```

## User-defined operators

User-defined infix operators can be defined by enclosing names in parentheses.

For example, to create an infix synonym for the [`Data.Array.range`](https://github.com/purescript/purescript-arrays) function:

``` purescript
(..) = Data.Array.range
```

This function can be used as follows::

```purescript
oneToTen = 1 .. 10
```

The associativity and precedence level of operators can be defined with the `infix` (no associativity), `infixl` (left associative), and `infixr` (right associative) top-level declarations:

```purescript
infix 5 ..
infixl 7 %%
infixr 9 ^^
```

See [[Understanding fixity declarations]] for more information about these declarations.

## If-Then-Else expressions

The `if`, `then` and `else` keywords can be used to create conditional expressions similar to a JavaScript ternary expression. The `else` block is always required:

``` purescript
conditional = if 2 > 1 then "ok" else "oops"
```

## Let and where bindings

The `let` keyword a collection of local declarations, which may be mutually recursive, and which may include type declarations::

``` purescript
factorial :: Int -> Int
factorial =
  let
    go :: Int -> Int -> Int
    go acc 1 = acc
    go acc n = go (acc * n) (n - 1)
  in
    go 1
```

The `where` keyword can also be used to introduce local declarations at the end of a value declaration:

``` purescript
factorial :: Number -> Number
factorial = go 1
  where
  go :: Number -> Number -> Number
  go acc 1 = acc
  go acc n = go (acc * n) (n - 1)
```

## Do notation

The `do` keyword introduces simple syntactic sugar for monadic expressions.

Here is an example, using the monad for the [`Maybe`](https://github.com/purescript/purescript-maybe) type:

``` purescript
maybeSum :: Maybe Number -> Maybe Number -> Maybe Number
maybeSum a b = do
  n <- a
  m <- b
  let result = n + m
  return result
```

`maybeSum` takes two values of type ``Maybe Number`` and returns their sum if neither number is `Nothing`.

When using `do` notation, there must be a corresponding instance of the `Monad` type class for the return type.

Statements can have the following form:

- `a <- x` which desugars to `x >>= \a -> ...`
- `x` which desugars to `x >>= \_ -> ...` or just `x` if this is the last statement.
- A let binding `let a = x`. Note the lack of the `in` keyword.

The example `maybeSum` desugars to::

``` purescript
maybeSum a b =
  a >>= \n ->
    b >>= \m ->
      let result = n + m
      in return result
```

# Type Classes

PureScript has basic support for type classes via the ``class`` and ``instance`` keywords. 

Types appearing in class instances are must be of the form ``String``, ``Number``, ``Boolean``, or ``C t1 ... tn`` where ``C`` is a type constructor (including ``[]`` and ``->`` and ``t_i`` are types of the same form).

Type class instances are resolved based on the order in which they appeared in the source files. Overlapping instances will result in a compilation error.

Here is an example of the ``Show`` typeclass, with instances for ``String``, ``Booleans`` and ``[]``::

  class Show a where
    show :: a -> String
  
  instance showString :: Show String where
    show s = s
  
  instance showBoolean :: Show Boolean where
    show true = "true"
    show false = "false"
  
  instance showArray :: (Show a) => Show [a] where
    show xs = "[" ++ joinWith ", " (map show xs) ++ "]"
  
  example = show [true, false]

Superclasses
------------

Superclass implications can be indicated in a class declaration with a backwards fat arrow ``<=``::

  class (Monad m) <= MonadFail m where
    fail :: forall a. String -> m a
    
Superclass instances will be used when searching for an instance of a subclass. For example, in the code below, the ``Monad`` constraint introduced by the ``return`` function can be discharged since ``Monad`` is a superclass of ``MonadFail``::

  assert :: forall m. (MonadFail m) => Boolean -> m Unit
  assert true = return unit
  assert false = fail "Assertion failed"
  
## Type Annotations

A constrained type will never be inferred for a top-level declaration without an explicit type signature. 

# Pattern Matching

Pattern matching deconstructs a value to bring zero or more expressions into scope. Pattern matches are introduced with the `case` keyword.

Pattern matches have the following general form::

```purescript
case value of
  pattern -> result
  ...
  pattern -> result
```

Pattern matching can also be used in the declaration of functions, as we have already seen::

```purescript
fn pattern_1 ... pattern_n = result
```

Patterns can also be used when introducing functions. For example::

```purescript
example x y z = x * y + z
```

The following pattern types are supported:

- Wildcard pattern
- Literal patterns
- Variable pattern
- Array patterns
- Constructor patterns
- Record patterns
- Named patterns
- Guards

Patterns need not be exhaustive. A pattern match failed exception will be thrown at runtime if no pattern matches the input. Patterns which are not exhaustive will generate warnings at compile time, however.

Wildcard Patterns
-----------------

The wilcard `_` matches any input and brings nothing into scope::

```purescript
f _ = 0
```
      
Literal Patterns
----------------

Literal patterns are provided to match on primitives::

```purescript
f true = 0
f false = 1
    
g "Foo" = 0
g _ = 1
  
h 0 = 0
h _ = 1
```

Variable Patterns
-----------------

A variable pattern matches any input and binds that input to its name::

  double x = x * 2

Array Patterns
--------------

Array patterns match an input which is an array, and bring its elements into scope. For example::

```purescript
f [x] = x
f [x, y] = x * y
f _ = 0
```

Here, the first pattern only matches arrays of length one, and brings the first element of the array into scope.

The second pattern matches arrays with two elements, and brings the first and second elements into scope.

Constructor patterns
--------------------

Constructor patterns match a data constructor and its arguments::

```purescript
data Foo = Foo String | Bar Number Boolean

foo (Foo s) = true
foo (Bar _ b) = b
```

Record Patterns
---------------

Record patterns match an input which is a record, and bring its properties into scope::

```purescript
f { foo = "Foo", bar = n } = n
f _ = 0
```

Nested Patterns
---------------

The patterns above can be combined to create larger patterns. For example::

```purescript
f { arr = [x, _], take = "firstOfTwo" } = x
f { arr = [_, x, _] take = "secondOfThree" } = x
f _ = 0
```

Named Patterns
--------------

Named patterns bring additional names into scope when using nested patterns. Any pattern can be named by using the ``@`` symbol::

```purescript
f a@[_, _] = true
f _ = false
```
     
Here, in the first pattern, any array with exactly two elements will be matched and bound to the variable ``a``.

Guards
------

Guards are used to impose additional constraints inside a pattern using boolean-valued expressions, and are introduced with a pipe after the pattern::

```purescript
evens :: List Int -> Int
evens Nil = 0
evens (Cons x xs) | x % 2 == 0 = 1 + evens xs
evens (Cons _ xs) = evens xs
```

When using patterns to define a function at the top level, guards appear after all patterns::

```purescript
greater x y | x > y = true
greater _ _ = false
```

# Modules

All code in PureScript is contained in a module. Modules are introduced using the ``module`` keyword::

```purescript
module A where
  
id x = x
```

When referencing values or data types in another module, names may be qualified by using a dot::

```purescript
module B where
  
foo = A.id
```

Importing Modules
-----------------

A module can be imported using the ``import`` keyword. This will create aliases for all of the values and types in the imported module::

```purescript
module B where
  
import A
```

Alternatively, a list of names to import can be provided in parentheses::

```purescript
module B where
  
import A (runFoo)
```

Values, type constructors and data constructors can all be explicitly imported. A type constructor should be followed by a list of associated data constructors to import in parentheses. A double dot (``..``) can be used to import all data constructors for a given type constructor::

```purescript
module B where

import A (runFoo, Foo(..), Bar(Bar))
```
  
Qualified Imports
-----------------
  
Modules can also be imported qualified, which means that their names will not be brought directly into scope, but rather, aliased to a different module name. This can be helpful when avoiding naming conflicts::

```purescript
module Main where
  
import Data.Array as Array
  
null = ...
```
  
Here, the name ``null`` would ordinarily conflict with ``null`` from ``Data.Array``, but the qualified import solves this problem. ``Data.Array.null`` can be referenced using ``Array.null`` instead.

Module Exports
--------------

Module exports can be restricted to a set of names by providing that set in parentheses in the module declaration::

```purescript
module A (runFoo, Foo(..)) where
```

The types of names which can be exported is the same as for module imports.

Currently, individual imported values and types cannot be re-exported by a module. However, as of PureScript release 0.7, imported modules can be re-exported in their entirety:

```purescript
module A (module B) where
import B
```
When re-exporting other modules, all local values and types can also be exported by specifying the module itself as an export:

```purescript
module A (module A, module B) where
import B
data ...
```

# FFI

Importing Values
----------------

The ``foreign import`` keywords declare a value which is defined in Javascript, and its type::

```purescript
foreign import pow :: Number -> Number -> Number
```

Importing Types
---------------

To declare a new abstract type (with no constructors), use ``foreign import data`` and provide the kind::

```purescript
foreign import data DOM :: *
  	
foreign import document :: { 
  createElement :: String -> DOM  
}
```

# Records

Record literals are surrounded by braces, as in JavaScript:

```purescript
author :: { name :: String, interests :: Array String }
author =
    { name: "Phil"
    , interests: ["Functional Programming", "JavaScript"]
    }
```

Fields of records can be accessed using a dot, followed by the label of the field to access:

```purescript
> author.name
"Phil"

> author.interests
["Functional Programming","JavaScript"]
```

## Kinds

`{ ... }` is just syntactic sugar for the `Object` type constructor, so `{ language ::  String }` is the same as `Object ( language :: String )`.

The Object type constructor is parameterized by a row of types. In kind notation, `Object` has kind `# * -> *`. That is, it takes a row of types to a type.

`( language :: String )` denotes a row of types (something of kind `# *`), so it can be passed to `Object` to construct a type, namely `Object ( language :: String )`.

## Extending Records

It is possible to define an extensible record

```purescript
type Lang l = { language :: String | l }
```

that can then be extended like:

```purescript
type Language = Lang ( country :: String )
```

The `Language` type synonym would then be equivalent to `{ language :: String, country :: String }`.

## Wildcards

Record literals with wildcards can be used to create a function that produces the record instead:

```purescript
{ foo: _, bar: _ }
```
is equivalent to:

```purescript
\foo bar -> { foo: foo, bar: bar }
```

## Record Update

PureScript also provides a record update syntax similar to Haskell's:

```purescript
setX :: Number -> Point -> Point
setX val point = point { x = val }
```
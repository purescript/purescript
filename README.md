purescript
==========

[![Build Status](https://secure.travis-ci.org/paf31/purescript.png?branch=master)](http://travis-ci.org/paf31/purescript)

A small strongly, statically typed compile-to-JS language with basic extensible records and type-safe blocks

## Current Features

- Full Type Inference
- Higher Kinded Polymorphism
- Support for basic Javascript types
- Extensible records
- Type-safe blocks with for/while/assignment etc.
- Functions with multiple arguments
- Pattern matching
- Simple FFI

## License

See the `LICENSE` file

## Installation

Install the Haskell Plaform, then `cabal install`

## Usage

The `psc` executable takes a list of PureScript source files as arguments and by default writes out its errors or output to the console.

The following options are supported:

    -o --output 
        Write the generated Javascript to the specified file
        
    -e --externs
        Write a list of externs declarations to the specified file in addition to generating Javascript output

## Motivation

I was looking for a simple functional language which would compile to JavaScript and have the following characteristics:

- Generate simple readable Javascript
- Provide the ability to compile to tight loops if necessary
- Reasonable type system
- Ideally, written in a programming language I would enjoy working in
- Provides a simple interface to existing Javascript code

I didn't find exactly what I was looking for, so I wrote PureScript. It doesn't have everything right now, but it should serve as a simple core on which to develop new ideas.

PureScript is *not* designed to be a general-purpose programming language. The primary use case is as a generator for purely-functional core libraries, with the main application code written in another language.

PureScript can also be seen as a trade-off between a theoretically ideal language and one which generates reasonably high performance code.

## First Example

The following code defines a `Person` data type and a function to generate a string representation for a `Person`:

    data Person = Person 
      { name :: String
      , age :: Number 
      }
	
    extern itoa :: Number -> String
	
    showPerson :: Person -> String
    showPerson = \p -> case p of
      Person o -> o.name ++ ", aged " ++ itoa(o.age)

Line by line, this reads as follows:

- `Person` is a data type with one constructor, also called `Person`
    - The `Person` constructor takes an object with two properties, `name` which is a `String`, and `age` which is a `Number`
- The `itoa` function is written in Javascript, and takes a `Number` and returns a `String`
- The `showPerson` function takes a `Person` and returns a `String`
- `showPerson` works by case analysis on its argument `p`, first matching the constructor `Person` and then using string concatenation and object accessors to return its result.

The generated Javascript looks like this:

    var Person = function (value) { 
      return { ctor: 'Person', value: value }; 
    };
    
    var showPerson = function (p) { 
      return (function (_0) {
        if (_0.ctor === "Person") { 
          var _1 = _0.value; 
          var o = _1; 
          return o.name ++ ", aged " ++ itoa(o.age); 
        }
        throw "Failed pattern match"; 
      })(p); 
    };

## Type System

The type system defines the following types:

- Primitive Types
    - Number
    - String
    - Boolean
- Arrays 
    - E.g. `[String]`, `[[Number]]`
- Records
    - E.g. `{ foo :: String ; bar :: Number }`
- Tagged Unions
    - E.g. `data Foo a = Foo | Bar String`
- Functions
    - E.g. `Number -> String`
    - E.g. `(Number, Number) -> Number`
    - Functions can have zero or more arguments
- Polymorphic types (for top level declarations only)
    - E.g. `forall a. a -> a` 

## Primitive Types

The three primitive types `String`, `Number` and `Boolean` correspond to their Javascript equivalents at runtime.

PureScript supports the same binary and unary operations on primitive types as Javascript, with the following exceptions:

- String concatenation is denoted `++` to differentiate it from numeric addition, `+`
- PureScript's `==` and `!=` correspond to Javascript's strong equality tests `===` and `!==`

Examples:

    num = 1 + 2 * 3
    str = "Hello, " ++ "World!"
    bool = 1 > 2 || true

## Arrays

PureScript arrays correspond to Javascript arrays at runtime, but all elements must have the same type.

Array literals look like Javascript array literals: `[1, 2, 3]`

Array elements can be read using array index notation `arr[0]`

## Records

PureScript records correspond to Javascript objects.

Record literals look like Javascript object literals: `{ foo: "Foo" }`

Properties can be read by using dot notation: `o.foo`

## Tagged Unions

Tagged unions consist of one or more constructors, each of which takes zero or one arguments.

Tagged unions can only be created using their constructors, and deconstructed through pattern matching (see later).

For example:

    data Foo a = Foo | Bar String
    
    runFoo = \foo -> case foo of
      Foo -> "It's a Foo"
      Bar s -> "It's a Bar. The string is " ++ s
      
    runFoo Foo ++ runFoo (Bar "Test")
    
In the example, Foo is a tagged union type which has two constructors. It's first constructor `Foo` takes no argument, and it's second `Bar` takes one, which must be a String.

`runFoo` is an example of pattern matching on a tagged union type to discover its constructor, and the last line shows how `Foo`s are constructed.

## Functions

Functions in TypeScript can have zero or more arguments in general, just like in Javascript.

Functions are introduced by using a backslash followed by a comma separated list of argument names:

    test1 = \a, b, c -> a + b + c
    
which would correspond to the Javascript `function test1(a, b, c) { return a + b + c; }`
    
A function taking no arguments would look like this:

    test2 = \ -> 100
    
which would correspond to the Javascript `function test2() { return 100; }`

Functions are applied by providing their arguments inside parentheses:

    test1(1, 2, 3)
    test2()
    
A special case is made in the case of functions with one argument. These functions can be applied without parentheses, and function application associates to the left:

    -- has type Number -> Number -> Number -> Number
    addThree = \a -> \b -> \c -> a + b + c
    
    -- has type Number -> Number -> Number
    addThree 1 
    
    -- has type Number -> Number
    addThree 1 2 
    
    -- has type Number
    addThree 1 2 3 

## Polymorphic Types

Expressions defined at the top level may have polymorphic types.

Here is an example:

    identity = \x -> x
    
`identity` is inferred to have (polymorphic) type `forall t0. t0 -> t0`. This means that for any type `t0`, `identity` can be given a value of type `t0` and will give back a value of the same type.

A type annotation can also be provided:

    identity :: forall a. a -> a
    identity = \x -> x
    
Functions may also be polymorphic in row types or type variables with other kinds (see "Kind System"):

    addProps = \o -> o.foo + o.bar
    
Here, `addProps` is inferred to have type `forall r. { foo :: Number; bar :: Number | r } -> Number`. That is, it can take any type which has properties `Foo` and `Bar`, and *any other record properties*.

So, the following compiles:

    addProps { foo: 1, bar: 2, baz: 3 }
    
but the following does not:

    addProps { foo: 1 }
    
since the `bar` property is missing.

Again, a type annotation can be provided if necessary.

## Type Inference

All types can be inferred, but annotations can optionally be provided.

## Kind System

There are two primitive kinds, the kind `*` of types and the kind `#` of rows. Higher kinded types are also supported. That is, a type variable can refer to not only a type or a row, but a type constructor, or row constructor etc.

## Whitespace

Syntax is whitespace sensitive. The general rule of thumb is that declarations which span multiple lines should be indented past the column on which they were first defined on their subsequent lines.

That is, the following is valid:

    foo = bar(x) + 
      baz(x)

But this is not:

    foo = bar(x) + 
    baz(x)

## Do Notation

There is a keyword `do`, which does not have the same purpose as the `do` keyword in Haskell.

`do` introduces a block. Blocks consist of statements, and must return a value of the same type on every branch of execution.

The following types of statement are supported:

- Variable introduction
- Variable assignment
- For loops
- For-each loops
- While loops
- If-Then-Else statements

Here is an example of a power function defined using a block:

    pow = \n, p -> do
	    var m = n
		  for i <- 0 until p:
		    m = m * n
		  return m

Blocks enable local mutation of their variables, but mutation is not allowed in general. The type system prevents mutable variables from escaping their scope.

That is, while the example above is valid, the following does not compile:

    incr = \n -> do
		  n = n + 1 
		  return n

The variable `n` is not mutable, and so the assignment in the first line of the `do` block is not allowed.

This function can be rewritten as follows:

    incr = \n -> do
      var m = n
      m = m + 1 
      return m

## For Loops

For loops look like this:

    total = do
      var n = 0
      for i <- 0 until 10:
        n = n + i
      return n

The bounds `0` and `10` are inclusive and exclusive respectively.
      
## For-Each Loops

For each loops loop over the elements in an array using the `Object.forEach` method. A polyfill may be required for some browsers:

    total = \arr -> do
      var n = 0
      foreach i in arr:
        n = n + i
      return n

## While Loops

The syntax of a while loop is similar to a foreach loop:

    log2 = \n -> do
      var count = 0
      var m = n
      while m > 1:
        m = m / 2
        count = count + 1
      return count

## If-Then-Else Statements

Else branches are optional, and may contain further `if` statements, just as in Javascript:

    collatz = \n -> do
      var count = 0
      var m = n
      while m > 1:
        if m % 2 == 0:
          m = m / 2
        else:
          m = m * 3 + 1
        count = count + 1
      return count
      
## If-Then-Else Expressions

The `if`, `then` and `else` keywords can also be used to create conditional expressions. In this case, the `else` block is always required.

For example,

    conditional = if 2 > 1 then "ok" else "oops"

## Pattern Matching

Pattern matching deconstructs a value to bring zero or more expressions into scope. Pattern matches are introduced with the `case` keyword.

Pattern matches have the following general form

    case value of
      pattern -> result
      ...
      pattern -> result

The following pattern types are supported:

- Wildcard pattern
- Literal patterns
- Variable pattern
- Array patterns
- Record patterns
- Named patterns
- Guards

Patterns need not be exhaustive. A pattern match failed exception will be thrown at runtime if no pattern matches the input.

## Wildcard Patterns

The wilcard `_` matches any input and brings nothing into scope:

    case x of 
      _ -> 0
      
## Literal Patterns

Literal patterns are provided to match on primitives:

    case booleanValue of 
      true -> 0
      false -> 1
      
    case stringValue of 
      "Foo" -> 0
      _ -> 1
      
    case numericValue of 
      0 -> 0
      _ -> 1

## Variable Pattern

A variable pattern matches any input and binds that input to its name:

    case foo(x) of
      result -> bar(result)

## Array Patterns

Array patterns match an input which is an array, and bring its elements into scope.

For example:

    f = \arr -> case arr of
      [x] -> x
      [x, y : xs] -> x * y + f(xs)
      _ -> 0

Here, the first pattern only matches arrays of length one, and brings the first element of the array into scope.

The second pattern matches arrays with two or more elements, and brings the first and second elements into scope, along with the remainder of the array as the variable `xs`.

## Record Patterns

Record patterns match an input which is a record, and bring its properties into scope.

    f = \o -> case o of
      { foo = "Foo" } -> o.bar
      _ -> 0

## Nested Patterns

The patterns above can be combined to create larger patterns.

For example:

    f = \o -> case o of
      { arr = [x:_], take = "car" } -> x
      { arr = [_,x:_], take = "cadr" } -> x
      _ -> 0

## Named Patterns

Named patterns bring additional names into scope when using nested patterns. Any pattern can be named by using the `@` symbol:

    f = \arr -> case arr of
      a@[_,_:_] -> a
      a -> a
      
Here, in the first pattern, any array with two or more elements will be matched and bound to the variable `a`.

## Guards

Guards are used to impose additional constraints inside a pattern using boolean-valued expressions, and are introduced with a pipe after the pattern:

    evens = \arr -> case arr of 
      [] -> 0
      [x:xs] | x % 2 == 0 -> 1 + evens xs
      [_:xs] -> evens xs
      
## Type Synonyms

For convenience, it is possible to declare a synonym for a type using the `type` keyword. Type synonyms can include type arguments.

For example:

    type Foo = { foo :: Number, bar Number }
    
    addFoo :: Foo -> Number
    addFoo = \o -> o.foo + o.bar
      
## Record Updates

Properties on records can be updated using the following syntax: `o { key = value, ..., key = value }`

For example, the following function increments the `foo` property on its argument:

    incr = \o -> o { foo = o.foo + 1 }

The generated Javascript assumes the existence of a method called `Object.extend` such that `Object.extend(o, p)` takes an object `o` and generates a shallow copy of `o` including the properties of `p`. A simple JQuery implementation of this specification is

    Object.prototype.extend = function(o, p) {
      return $.extend({}, o, p);
    };

## Operators

In addition to the standard operators, user-defined infix operators can be created by enclosing names in parentheses:

E.g. to create a synonym for string concatenation:

    (<>) = \s1 -> \s2 -> s1 ++ s2
	
    greeting = "Hello" <> "World!"
	  
Regular functions can be used as operators by enclosing their names in backticks:

    foo = \x -> \y -> x * y + y
    
    test = 10 `foo` 20
    
Fixity declarations can associate a precedence level, which is a natural number, to a user-defined operator, and specify which way it associates:

    infixl 5 <>
    infixr 7 %%

## Foreign Function Interface

The `extern` keyword declares a value which is defined in Javascript, and its type:

    extern pow :: (Number, Number) -> Number
	
To declare a new type with no constructors, use `extern data` and provide the kind:

    extern IO :: * -> *
	
    extern console :: { 
      log :: String -> IO {} 
    }

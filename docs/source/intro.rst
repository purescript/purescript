Introduction
============

PureScript is a small strongly, statically typed compile-to-JS language with a number of interesting features, such as:

- Type Inference
- Higher Kinded Polymorphism
- Support for basic Javascript types
- Extensible records
- Extensible effects
- Type-safe blocks with for/while/assignment etc.
- Functions with multiple arguments
- Pattern matching
- Simple FFI
- Modules
- Rank N Types
- Do Notation
- Optional tail-call elimination
- Type Classes

Try It!
-------

Try PureScript in the browser [here](http://tryps.functorial.com/).

Installation
------------

Install the Haskell Plaform, then `cabal install`

Usage
-----

The `psc` executable takes a list of PureScript source files as arguments and by default writes out its errors or output to the console.

The following options are supported:

-s --stdin             Read input from standard input instead of from files
-o --output            Write the generated Javascript to the specified file
-e --foreign imports   Write a list of foreign imports declarations to the specified file in addition to generating Javascript output
--runtime-type-checks  Generate simple runtime type checks for function arguments with simple types.
--tco                  Perform tail-call elimination on the generated Javascript.
--no-prelude           Do not include the Prelude in the generated Javascript.
--magic-do             Overload the `do` keyword to inline calls to `bind` for the `Eff` monad, to generate more efficient code.
--run-main             Generate a call to `Main.main` after all other generated Javascript.

Motivation
----------

I was looking for a simple functional language which would compile to JavaScript and have the following characteristics:

- Generate simple readable Javascript
- Provide the ability to compile to tight loops if necessary
- Reasonable type system
- Ideally, written in a programming language I would enjoy working in
- Provides a simple interface to existing Javascript code

I didn't find exactly what I was looking for, so I wrote PureScript. It doesn't have everything right now, but it should serve as a simple core on which to develop new ideas.

PureScript is *not* designed to be a general-purpose programming language. The primary use case is as a generator for purely-functional core libraries, with the main application code written in another language.

PureScript can also be seen as a trade-off between a theoretically ideal language and one which generates reasonably high performance code.

Hello, PureScript!
------------------

As an introductory example, here is the usual "Hello World" written in PureScript::

  module Main where
  
  import Trace
  
  main = trace "Hello, World!"

which compiles to the following Javascript, ignoring the Prelude::

  var Main;
  (function (Main) {
      var main = trace("Hello, World!");
      Main.main = main;
  })(Main = Main || {});

The following command will compile and execute the PureScript code above::

  psc input.purs --run-main | nodejs

Another Example
---------------

The following code defines a `Person` data type and a function to generate a string representation for a `Person`::

  data Person = Person { name :: String, age :: Number }
  
  foreign import numberToString :: Number -> String
  
  showPerson :: Person -> String
  showPerson (Person o) = o.name ++ ", aged " ++ numberToString(o.age)

Line by line, this reads as follows:

- `Person` is a data type with one constructor, also called `Person`
    - The `Person` constructor takes an object with two properties, `name` which is a `String`, and `age` which is a `Number`
- The `numberToString` function is written in Javascript, and converts a `Number` to its `String` representation
- The `showPerson` function takes a `Person` and returns a `String`
- `showPerson` works by case analysis on its argument, first matching the constructor `Person` and then using string concatenation and object accessors to return its result.

The generated Javascript looks like this::

  var Person = function (value) { 
      return { ctor: 'Person', value: value }; 
  };
  
  function showPerson(_1) {
      return _1.value.name + ", aged " + numberToString(_1.value.age); 
  };

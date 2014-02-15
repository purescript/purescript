Introduction
============

PureScript is a small strongly, statically typed compile-to-JS language with a number of interesting features, such as:

- Type Inference
- Higher Kinded Polymorphism
- Support for basic Javascript types
- Extensible records
- Extensible effects
- Type-safe blocks with for/while/assignment etc.
- Pattern matching
- Simple FFI
- Modules
- Rank N Types
- Do Notation
- Optional tail-call elimination
- Type Classes

Installation
------------

If you have the Haskell Plaform installed, then you can install the latest released version from Hackage::

  cabal update
  cabal install purescript

If you would like to build the latest version of the code, clone this repository and build::

  git clone git://github.com:paf31/purescript.git
  cabal configure --enable-tests
  cabal build
  cabal test
  cabal install

Usage
-----

The `psc` executable takes a list of PureScript source files as arguments and by default writes out its errors or output to the console.

The following options are supported:

--stdin                Read input from standard input instead of from files
--output               Write the generated Javascript to the specified file
--externs              Write a list of foreign imports declarations to the specified file in addition to generating Javascript output
--runtime-type-checks  Generate simple runtime type checks for function arguments with simple types.
--tco                  Perform tail-call elimination on the generated Javascript.
--no-prelude           Do not include the Prelude in the generated Javascript.
--magic-do             Overload the `do` keyword to inline calls to `bind` for the `Eff` monad, to generate more efficient code.
--main                 Generate a call to `main` in the specified module after all other generated Javascript. Defaults to `Main` if the option is used but no value is provided.
--module               If specified, any code which is not referenced transitively from this module will be removed. This argument can be used multiple times.
--browser-namespace    Specify the namespace that PureScript modules will be exported to when running in the browser.

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
      return { ctor: 'Person', values: [value] }; 
  };
  
  function showPerson(_1) {
      return _1.values[0].name + ", aged " + numberToString(_1.values[0].age); 
  };

Related Projects
----------------

PureScript might be compared to other AltJS projects such as Roy, Haste, Fay, Elm and GHCJS. Certainly, there is a lot of overlap in terms of syntax, but the goals of PureScript listed above separate it in one or more ways from each of these languages.

Roy is probably the most similar language on the list, and was a large influence on the development of PureScript. There are however, key differences in the foreign function interface, the type system and the choice of development language (Haskell vs. Javascript)

Projects such as Haste, Fay and GHCJS aim to use some combination of the GHC compiler itself and/or its intermediate representation, Core, to perform some of the tasks involved in compilation such as parsing and type checking. This usually gives the advantage that tools and libraries can be shared with Haskell, but often at the cost of the size of the generated Javascript. This is the main practical difference between PureScript and these projects.

Elm also shares a lot in terms of functionality with PureScript. Elm is designed for functional reactive programming, and focusses on tools and language features suitable for that domain, while PureScript focusses on the development of purely functional core application logic. Another difference between PureScript and Elm is PureScript's lack of a runtime system.


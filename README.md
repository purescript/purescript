purescript
==========

[![Build Status](https://secure.travis-ci.org/paf31/purescript.png?branch=master)](http://travis-ci.org/paf31/purescript)

A small strongly, statically typed compile-to-JS language with basic extensible records and type-safe blocks

Bitcoin donations are gratefully accepted at 14ZhCym28WDuFhocP44tU1dBpCzjX1DvhF.

## Current Features

- Full Type Inference
- Higher Kinded Polymorphism
- Support for basic Javascript types
- Extensible records
- Type-safe blocks with for/while/assignment etc.
- Functions with multiple arguments
- Pattern matching
- Simple FFI
 
## Experimental Features

- Modules
- Rank N Types

## Try It!

Try PureScript in the browser [here](http://tryps.functorial.com/).

## License

See the `LICENSE` file

## Contributing

Contributions are always encouraged! Please see `CONTRIBUTING.md` for guidelines.

## Installation

Install the Haskell Plaform, then `cabal install`

## Usage

The `psc` executable takes a list of PureScript source files as arguments and by default writes out its errors or output to the console.

The following options are supported:

    -o --output 
        Write the generated Javascript to the specified file
        
    -e --foreign imports
        Write a list of foreign imports declarations to the specified file in addition to generating Javascript output

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

## Hello, PureScript!

As an introductory example, here is the usual "Hello World" written in PureScript:

```haskell
foreign import console :: { log :: String -> {} }

main = \ -> console.log "Hello, World!"
```

which compiles to the following Javascript:

```javascript
function main() {
    console.log("Hello, World!");
}
```

## Another Example

The following code defines a `Person` data type and a function to generate a string representation for a `Person`:

```haskell
data Person = Person { name :: String, age :: Number }

foreign import numberToString :: Number -> String

showPerson :: Person -> String
showPerson (Person o) = o.name ++ ", aged " ++ numberToString(o.age)
```

Line by line, this reads as follows:

- `Person` is a data type with one constructor, also called `Person`
    - The `Person` constructor takes an object with two properties, `name` which is a `String`, and `age` which is a `Number`
- The `numberToString` function is written in Javascript, and converts a `Number` to its `String` representation
- The `showPerson` function takes a `Person` and returns a `String`
- `showPerson` works by case analysis on its argument, first matching the constructor `Person` and then using string concatenation and object accessors to return its result.

The generated Javascript looks like this:

```javascript
var Person = function (value) { 
    return { ctor: 'Person', value: value }; 
};

function showPerson(_1) {
    if (_1.ctor === "Person") {
        return _1.value.name + ", aged " + numberToString(_1.value.age); 
    }; 
    throw "Failed pattern match"; 
}; 

```

## Type System

The type system defines the following types:

- Primitive Types
    - Number
    - String
    - Boolean
- Arrays 
    - E.g. `[String]`, `[[Number]]`
- Records
    - E.g. `{ foo :: String, bar :: Number }`
- Tagged Unions
    - E.g. `data Foo a = Foo | Bar String`
- Functions
    - E.g. `Number -> String`
    - E.g. `(Number, Number) -> Number`
    - Functions can have zero or more arguments
- Polymorphic types
    - E.g. `forall a. a -> a` 

## Primitive Types

The three primitive types `String`, `Number` and `Boolean` correspond to their Javascript equivalents at runtime.

PureScript supports the same binary and unary operations on primitive types as Javascript, with the following exceptions:

- String concatenation is denoted `++` to differentiate it from numeric addition, `+`
- PureScript's `==` and `!=` correspond to Javascript's strong equality tests `===` and `!==`

Examples:

```haskell
num = 1 + 2 * 3
str = "Hello, " ++ "World!"
bool = 1 > 2 || true
```

## Arrays

PureScript arrays correspond to Javascript arrays at runtime, but all elements must have the same type.

Array literals look like Javascript array literals: `[1, 2, 3]`

Array elements can be read using array index notation `arr !! index`

## Records

PureScript records correspond to Javascript objects.

Record literals look like Javascript object literals: `{ foo: "Foo" }`

Properties can be read by using dot notation: `o.foo`

## Tagged Unions

Tagged unions consist of one or more constructors, each of which takes zero or one arguments.

Tagged unions can only be created using their constructors, and deconstructed through pattern matching (see later).

For example:

```haskell
data Foo a = Foo | Bar String

runFoo Foo = "It's a Foo"
runFoo (Bar s) = "It's a Bar. The string is " ++ s

test = runFoo Foo ++ runFoo (Bar "Test")
```

In the example, Foo is a tagged union type which has two constructors. It's first constructor `Foo` takes no argument, and it's second `Bar` takes one, which must be a String.

`runFoo` is an example of pattern matching on a tagged union type to discover its constructor, and the last line shows how `Foo`s are constructed.

## Functions

Functions in PureScript can have zero or more arguments in general, just like in Javascript.

Functions are introduced by using a backslash followed by a list of argument names:

```haskell
test1 = \a b -> a + b
```

which would correspond to the Javascript 

```javascript
function test1(a) {
  return function (b) { 
    return a + b;
  }
}
```

Multiple argument functions can be introduced by wrapping the arguments in parentheses, and separating them with commas:

```haskell
test1 = \(a, b) -> a + b
```

which generates

```javascript
function test1(a, b) { 
  return a + b;
}
```
    
In the case of a function with no arguments, the parentheses may be omitted, as follows:

```haskell
test2 = \ -> 100
```

which would correspond to the Javascript `function test2() { return 100; }`

Multiple-argument and single-argument syntax can be mixed, as follows:

```haskell
test3 = \a (b, c) d -> a + b + c + d
```

which generates

```javascript
function test3(a) {
    return function (b, c) {
        return function (d) {
            return a + b + c + d;
        }
    }
}
```

Functions are applied by providing their arguments inside parentheses:

```haskell
test1(1, 2, 3)
test2()
```

A special case is made in the case of functions with one argument. These functions can be applied without parentheses, and function application associates to the left:

```haskell
-- has type Number -> Number -> Number -> Number
addThree = \a b c -> a + b + c

-- has type Number -> Number -> Number
addThree 1 

-- has type Number -> Number
addThree 1 2 

-- has type Number
addThree 1 2 3 
```

## Polymorphic Types

Expressions defined at the top level may have polymorphic types.

Here is an example:

```haskell
identity x = x
```

`identity` is inferred to have (polymorphic) type `forall t0. t0 -> t0`. This means that for any type `t0`, `identity` can be given a value of type `t0` and will give back a value of the same type.

A type annotation can also be provided:

```haskell
identity :: forall a. a -> a
identity x = x
```

Functions may also be polymorphic in row types or type variables with other kinds (see "Kind System"):

```haskell
addProps o = o.foo + o.bar
```
    
Here, `addProps` is inferred to have type `forall r. { foo :: Number, bar :: Number | r } -> Number`. That is, it can take any type which has properties `Foo` and `Bar`, and *any other record properties*.

So, the following compiles:

```haskell
addProps { foo: 1, bar: 2, baz: 3 }
```
    
but the following does not:

```haskell
addProps { foo: 1 }
```
    
since the `bar` property is missing.

Again, a type annotation can be provided if necessary.

## Rank N Types (Experimental Feature)

It is also possible for the `forall` quantifier to appear on the left of a function arrow, inside types record fields and data constructors, and in type synonyms.

In most cases, a type annotation is necessary when using this feature.

As an example, we can pass a polymorphic function as an argument to another function:

```haskell
poly :: (forall a. a -> a) -> Boolean
poly f = (f 0 < 1) == f true
```

Notice that the polymorphic function's type argument is instantiated to both `Number` and `Boolean`.

An argument to `poly` must indeed be polymorphic. For example, the following fails:

```haskell
test = poly (\n -> n + 1)
```

since the skolemized type variable `a` does not unify with `Number`.

## Type Inference

All types can be inferred, but annotations can optionally be provided.

## Kind System

There are two primitive kinds, the kind `*` of types and the kind `#` of rows. Higher kinded types are also supported. That is, a type variable can refer to not only a type or a row, but a type constructor, or row constructor etc.

## Whitespace

Syntax is whitespace sensitive. The general rule of thumb is that declarations which span multiple lines should be indented past the column on which they were first defined on their subsequent lines.

That is, the following is valid:

```haskell
foo = bar(x) + 
  baz(x)
```

But this is not:

```haskell
foo = bar(x) + 
baz(x)
```

## Blocks

Blocks are collections of statements wrapped in braces `{ ... }`. Blocks must return a value of the same type on every branch of execution.

The following types of statement are supported:

- Variable introduction
- Variable assignment
- For loops
- For-each loops
- While loops
- If-Then-Else statements
- Naked expressions

Here is an example of a power function defined using a block:

```haskell
pow n p = {
    var m = n;
    for (i <- 0 until p) {
      m = m * n;
    }
    return m;
  }
```

Blocks enable local mutation of their variables, but mutation is not allowed in general. The type system prevents mutable variables from escaping their scope.

That is, while the example above is valid, the following does not compile:

```haskell
incr n = {
    n = n + 1;
    return n;
  }
```

The variable `n` is not mutable, and so the assignment in the first line of the `do` block is not allowed.

This function can be rewritten as follows:

```haskell
incr n = {
    var m = n;
    m = m + 1;
    return m;
  }
```

## For Loops

For loops look like this:

```haskell
total = {
    var n = 0;
    for (i <- 0 until 10) {
      n = n + i;
    }
    return n;
  }
```

The bounds `0` and `10` are inclusive and exclusive respectively.
      
## For-Each Loops

For each loops loop over the elements in an array using the `Object.forEach` method. A polyfill may be required for some browsers:

```haskell
total arr = {
    var n = 0;
    foreach (i in arr) {
      n = n + i;
    }
    return n;
  }
```

## While Loops

The syntax of a while loop is similar to a foreach loop:

```haskell
log2 n = {
    var count = 0;
    var m = n;
    while (m > 1) {
      m = m / 2;
      count = count + 1;
    }
    return count;
  }
```

## If-Then-Else Statements

Else branches are optional, and may contain further `if` statements, just as in Javascript:

```haskell
collatz n = {
    var count = 0;
    var m = n;
    while (m > 1) {
      if (m % 2 == 0) {
        m = m / 2;
      } else {
        m = m * 3 + 1;
      }
      count = count + 1;
    }
    return count;
  }
```

## Naked Expressions as Statements

Any expression with the empty type `{ }` can be used as a statement in a block. For example, method calls which return `{ }`:

```haskell
example n m = {
    console.log "Adding two numbers"
    return n + m;
  }
```
      
## If-Then-Else Expressions

The `if`, `then` and `else` keywords can also be used to create conditional expressions. In this case, the `else` block is always required.

For example,

```haskell
conditional = if 2 > 1 then "ok" else "oops"
```

## Pattern Matching

Pattern matching deconstructs a value to bring zero or more expressions into scope. Pattern matches are introduced with the `case` keyword.

Pattern matches have the following general form

```haskell
case value of
  pattern -> result
  -- ...
  pattern -> result
```

Pattern matching can also be used in the declaration of functions, as we have already seen:

```haskell
fn pattern_1 ... pattern_n = result
```

Any of the above types of pattern are also valid when introducing functions. In addition, patterns can also be grouped in parentheses to introduce multiple-arugument functions. For example, 

```haskell
example x (y, z) w = x * y + z * w
```

The following pattern types are supported:

- Wildcard pattern
- Literal patterns
- Variable pattern
- Array patterns
- Cons patterns
- Record patterns
- Named patterns
- Guards

Patterns need not be exhaustive. A pattern match failed exception will be thrown at runtime if no pattern matches the input.

## Wildcard Patterns

The wilcard `_` matches any input and brings nothing into scope:

```haskell
case x of 
  _ -> 0
```
      
## Literal Patterns

Literal patterns are provided to match on primitives:

```haskell
case booleanValue of 
  true -> 0
  false -> 1
  
case stringValue of 
  "Foo" -> 0
  _ -> 1
  
case numericValue of 
  0 -> 0
  _ -> 1
```

## Variable Pattern

A variable pattern matches any input and binds that input to its name:

```haskell
case foo(x) of
  result -> bar(result)
```

## Array Patterns

Array patterns match an input which is an array, and bring its elements into scope.

For example:

```haskell
f = \arr -> case arr of
  [x] -> x
  [x, y] -> x * y + f xs
  _ -> 0
```

Here, the first pattern only matches arrays of length one, and brings the first element of the array into scope.

The second pattern matches arrays with two elements, and brings the first and second elements into scope.

## Cons Patterns

The head and tail of a non-empty array can be matched by using a cons pattern:

```haskell
add = \arr -> case arr of
  [] -> 0
  x : xs -> x + add xs
```

`:` associates to the right:

```haskell
addPairs = \arr -> case arr of
  x : y : xs -> x * y + addPairs xs
  _ -> 0
```

## Record Patterns

Record patterns match an input which is a record, and bring its properties into scope.

```haskell
f = \o -> case o of
  { foo = "Foo" } -> o.bar
  _ -> 0
```

## Nested Patterns

The patterns above can be combined to create larger patterns.

For example:

```haskell
f = \o -> case o of
  { arr = x : _, take = "car" } -> x
  { arr = _ : x : _, take = "cadr" } -> x
  _ -> 0
```

## Named Patterns

Named patterns bring additional names into scope when using nested patterns. Any pattern can be named by using the `@` symbol:

```haskell
f = \arr -> case arr of
  a@(_ : _ : _) -> true
  a -> false
```
     
Here, in the first pattern, any array with two or more elements will be matched and bound to the variable `a`.

## Guards

Guards are used to impose additional constraints inside a pattern using boolean-valued expressions, and are introduced with a pipe after the pattern:

```haskell
evens = \arr -> case arr of 
  [] -> 0
  x : xs | x % 2 == 0 -> 1 + evens xs
  _ : xs -> evens xs
```

When defining a function, guards appear after all patterns:

```haskell
greater x y | x > y = true
greater _ _ = false
```

## Type Synonyms

For convenience, it is possible to declare a synonym for a type using the `type` keyword. Type synonyms can include type arguments.

For example:

```haskell
type Foo = { foo :: Number, bar Number }

addFoo :: Foo -> Number
addFoo = \o -> o.foo + o.bar
```
## Record Updates

Properties on records can be updated using the following syntax: `o { key = value, ..., key = value }`

For example, the following function increments the `foo` property on its argument:

```haskell
incr = \o -> o { foo = o.foo + 1 }
```

The generated Javascript assumes the existence of a method called `Object.extend` such that `Object.extend(o, p)` takes an object `o` and generates a shallow copy of `o` including the properties of `p`. A simple JQuery implementation of this specification is

```haskell
Object.prototype.extend = function(o, p) {
    return $.extend({}, o, p);
};
```

## Operators

In addition to the standard operators, user-defined infix operators can be created by enclosing names in parentheses:

E.g. to create a synonym for string concatenation:

```haskell
(<>) = \s1 s2 -> s1 ++ s2

greeting = "Hello" <> "World!"
```

Regular functions can be used as operators by enclosing their names in backticks:

```haskell
foo = \x y -> x * y + y

test = 10 `foo` 20
```
    
Fixity declarations can associate a precedence level, which is a natural number, to a user-defined operator, and specify which way it associates:

```haskell
infixl 5 <>
infixr 7 %%
```

## Modules

Simple modules are supported by the `module` keyword.

Introduce a module as follows, with a list of indented declarations:

```haskell
module A where

    foo = \x -> x
```

Modules may be nested:

```haskell
module A where

    foo = \x -> x
    
    module B where
    
        bar = \y -> y
```

Names may be qualified by using a dot:

```
foo = A.foo
bar = A.B.bar
```

All the names in a module can be aliased using the `import` declaration:

```haskell
import A
```

You can also limit which names are going to be aliased:

```haskell
import A.B (bar)
```

## Foreign Function Interface

The `foreign import` keyword declares a value which is defined in Javascript, and its type:

```haskell
foreign import pow :: (Number, Number) -> Number
```

To declare a new type with no constructors, use `foreign import data` and provide the kind:

```haskell
foreign import data IO :: * -> *
	
foreign import console :: { 
  log :: String -> IO {} 
}
```

To alias a name of a field defined on a Javascript type to a PureScript function, use `foreign import member`. For example, to define a function `length` which accesses the `length` member of an array:

```haskell
foreign import member "length" length :: forall a. [a] -> Number
```



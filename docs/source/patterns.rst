Pattern Matching
================

Pattern matching deconstructs a value to bring zero or more expressions into scope. Pattern matches are introduced with the `case` keyword.

Pattern matches have the following general form::

  case value of
    pattern -> result
    ...
    pattern -> result

Pattern matching can also be used in the declaration of functions, as we have already seen::

  fn pattern_1 ... pattern_n = result

Patterns can also be used when introducing functions. For example::

  example x y z = x * y + z

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

Wildcard Patterns
-----------------

The wilcard `_` matches any input and brings nothing into scope::

  f _ = 0
      
Literal Patterns
----------------

Literal patterns are provided to match on primitives::

  f true = 0
  f false = 1
    
  g "Foo" = 0
  g _ = 1
  
  h 0 = 0
  h _ = 1

Variable Patterns
-----------------

A variable pattern matches any input and binds that input to its name::

  double x = x * 2

Array Patterns
--------------

Array patterns match an input which is an array, and bring its elements into scope. For example::

  f [x] = x
  f [x, y] = x * y + f xs
  f _ = 0

Here, the first pattern only matches arrays of length one, and brings the first element of the array into scope.

The second pattern matches arrays with two elements, and brings the first and second elements into scope.

Cons Patterns
-------------

The head and tail of a non-empty array can be matched by using a cons pattern::

  sum [] = 0
  sum (x : xs) = x + sum xs

`:` associates to the right::

  addPairs (x : y : xs) = x * y + addPairs xs
  addPairs _ = 0

Record Patterns
---------------

Record patterns match an input which is a record, and bring its properties into scope::

  f { foo = "Foo" } = o.bar
  f _ = 0

Nested Patterns
---------------

The patterns above can be combined to create larger patterns. For example::

  f { arr = x : _, take = "car" } = x
  f { arr = _ : x : _, take = "cadr" } = x
  f _ = 0

Named Patterns
--------------

Named patterns bring additional names into scope when using nested patterns. Any pattern can be named by using the ``@`` symbol::

  f a@(_ : _ : _) = true
  f _ = false
     
Here, in the first pattern, any array with two or more elements will be matched and bound to the variable ``a``.

Guards
------

Guards are used to impose additional constraints inside a pattern using boolean-valued expressions, and are introduced with a pipe after the pattern::

  evens [] = 0
  evens (x : xs) | x % 2 == 0 = 1 + evens xs
  evens (_ : xs) = evens xs

When using patterns to define a function at the top level, guards appear after all patterns::

  greater x y | x > y = true
  greater _ _ = false

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

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
      
## If-Then-Else Expressions

The `if`, `then` and `else` keywords can also be used to create conditional expressions. In this case, the `else` block is always required.

For example,

```haskell
conditional = if 2 > 1 then "ok" else "oops"
```

## Do Notation

The `do` keyword introduces simple syntactic sugar for monadic expressions.

Here is an example, using the maybe monad:

```haskell
data Maybe a = Nothing | Just a

instance Monad Maybe where
  ret = Just
  (>>=) Nothing _ = Nothing
  (>>=) (Just a) f = f a

isEven n | n % 2 == 0 = Just {}
isEven _ = Nothing

evenSum a b = do
  n <- a
  m <- b
  let sum = n + m
  isEven sum
  ret sum
```

`isEven` adds two values of type `Maybe Number` and returns their sum, if the sum is even. If the sum is odd, `evenSum` returns `Nothing`.

This example illustrates the following aspects of `do` notation:

- The corresponding type constructor must be an instance of the `Prelude.Monad` type class, which defines the `ret` and `>>=` functions.
- Statements can have the following form:
    - `a <- x` which desugars to `m.bind x (\a -> ...)` 
    - `let a = x` which desugars to `(\a -> ...)(x)` 
    - `x` which desugars to `m.bind x (\_ -> ...)` or just `x` if this is the last statement.

Not illustrated here, but equally valid is the use of a binder on the left hand side of `<-` or `=`. For example:

```haskell
test arr = do
  (x:y:_) <- arr
  ret (x + y)
```

A pattern match failure will generate a runtime exception, just as in the case of a regular `case` statement.

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

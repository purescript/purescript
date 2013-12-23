## 0.2

### New Features

- RankNTypes

  This experimental feature enables the use of the `forall` quantifier in a 
- Modules
- Polymorphic Object Update

  Records now support member update in which the type of the field changes during the update. For example:

        data Wrap a = Wrap a
        
        update = \o -> o { prop = Wrap o.prop }

### Enhancements

- Allow method calls in blocks
- Split code generation into AST -> JS AST and JS AST -> String
- Syntactic sugar for introducing curried functions
- Perform some basic optimization on the generated Javascript.
- Generate formatted Javascript
- Syntax for creating an array from a list of heads and a tail

### Syntax Changes

- Avoid 'do' keyword for blocks
- Member extern syntax
- Make FFI syntax match Haskell
- Make record declaration syntax match Haskell
- Array pattern match syntax is confusing
- Array indexing syntax is ambiguous
- Optional parentheses in function declaration

### Bug Fixes

- Allow guards access to current scope

### Libraries

- Prelude


This error occurs when a pattern matching definition has **redundant** patterns, i.e., overlapping clauses.

For example, if you have something like:
```haskell
module Main where
not :: Boolean -> Boolean
not true = false
not _ = true
not false = true
```
you can notice that the third case of `not` is redundant, as it is covered by the second case (which, in particular, covers all cases).

So, the compiler will warn:
```haskell
 Warning in module Main:
 Warning in value declaration f:
 Warning at test.purs line 2, column 1 - line 3, column 1:
      Redundant cases have been detected.
      The definitions has the following redundant cases:

      false
```

For avoiding this messages, you will need to remove those redundant cases from your definitions.
Literals and arrays are not currently checked for redundancy.

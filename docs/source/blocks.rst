Blocks
======

Blocks are collections of statements wrapped in braces `{ ... }`. Blocks must return a value of the same type on every branch of execution.

The following types of statement are supported:

- Variable introduction
- Variable assignment
- For loops
- While loops
- If-Then-Else statements

Here is an example of a power function defined using a block::

  pow n p = {
      var m = n;
      for (i <- 0 until p) {
        m = m * n;
      }
      return m;
    }

Blocks enable local mutation of their variables, but mutation is not allowed in general. The type system prevents mutable variables from escaping their scope.

That is, while the example above is valid, the following does not compile::

  incr n = {
      n = n + 1;
      return n;
    }

The variable `n` is not mutable, and so the assignment in the first line of the `do` block is not allowed.

This function can be rewritten as follows::

  incr n = {
      var m = n;
      m = m + 1;
      return m;
    }

For Loops
---------

For loops iterate over a range of numbers::

  total = {
      var n = 0;
      for (i <- 0 until 10) {
        n = n + i;
      }
      return n;
    }

The bounds `0` and `10` are inclusive and exclusive respectively.

While Loops
------------

While loops repeat a set of statements while a boolean expression evaluates to `true`::

  log2 n = {
      var count = 0;
      var m = n;
      while (m > 1) {
        m = m / 2;
        count = count + 1;
      }
      return count;
    }

If-Then-Else Statements
-----------------------

Else branches are optional, and may contain further `if` statements, just as in Javascript:

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

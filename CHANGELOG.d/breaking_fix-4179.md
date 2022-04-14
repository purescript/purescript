* Lazy initialization for recursive bindings

  This is unlikely to break a working program, but the upshot for users is
  that it's now possible to get a run-time error when dereferencing an
  identifier in a recursive binding group before it has been initialized,
  instead of silently getting an `undefined` value and having that maybe
  or maybe not lead to an error somewhere else.

  This change can cause code that relies on tail-call optimization to no
  longer compile with that optimization. If you find that code that
  previously compiled to a TCO loop no longer does but does include `$lazy`
  initializers, please report the issue.

  **Alternate backend maintainers:** for you, this change represents a
  clarification of a responsibility shared by all backends. The identifiers
  bound in a recursive binding group need to behave as if those identifiers
  have call-by-need semantics during the initialization of the entire binding
  group. (Initializing the binding group entails ensuring every initializer
  has been executed, so after the binding group is initialized, these
  identifiers can be considered call-by-value again.)

  If an identifier is needed during its own call-by-need initialization, the
  backend must ensure that an explicit run-time error is raised appropriate for
  your target platform. This error may be raised at compile time instead if the
  backend can determine that such a cycle is inevitable. Returning your
  target language's equivalent of JavaScript's `undefined`, as `purs` did in
  earlier releases in some cases, is not permitted.

  If your target language natively has call-by-need semantics, you probably
  don't have to do anything. If your target language is call-by-value and you
  are using PureScript as a library, you can use the function
  `Language.PureScript.CoreFn.Laziness.applyLazinessTransform` to your CoreFn
  input to satisfy this responsibility; if you do, you will need to do the
  following:

    * Translate `InternalIdent RuntimeLazyFactory` and `InternalIdent (Lazy _)`
      identifiers to appropriate strings for your backend
    * Ensure that any output file that needs it has a reference to a function
      named `InternalIdent RuntimeLazyFactory`, with type `forall a. Fn3 String
      String (Unit -> a) (Int -> a)`, and with the same semantics as the
      following JavaScript (though you should customize the error raised to be
      appropriate for your target language):

      ```js
      function (name, moduleName, init) {
          var state = 0;
          var val;
          return function (lineNumber) {
              if (state === 2) return val;
              if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
              state = 1;
              val = init();
              state = 2;
              return val;
          };
      };
      ```
  
  If neither of the previous cases apply to you, you can meet this
  responsibility most easily simply by ensuring that all recursive bindings are
  lazy. You may instead choose to implement some light analysis to skip
  generating lazy bindings in some cases, such as if every initializer in the
  binding group is an `Abs`. You also may choose to reimplement
  `applyLazinessTransform`, or even develop a more sophisticated laziness
  transform for your backend. It is of course your responsibility to ensure
  that the result of whatever analysis you do is equivalent to the expected
  semantics.

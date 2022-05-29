* Allow leading and trailing delimiters in many syntax positions

  PureScript's syntax contains multiple places where sequences of words or
  clauses are expected, delimited by commas or vertical bars. To
  facilitate editing these elements when placed on their own lines,
  PureScript's parser now supports an optional leading or trailing
  delimiter in most such locations, specifically:

    * Array and object literals (and patterns)
    * Sum type definitions
    * Object updates
    * Row types
    * Import and export lists
    * Functional dependencies
    * Compound constraints and superclasses

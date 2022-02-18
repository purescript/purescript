* Scope type vars when type checking typed values

  When the compiler is checking an expression that is annotated with a
  type against another expected type, and the annotation introduces a type
  variable, the compiler needs to introduce that type variable to the
  scope of any types used inside the expression.

  One noteworthy case of this pattern is member signatures inside
  instances. This fix allows type variables introduced in member
  signatures to be used in the member declaration itself.

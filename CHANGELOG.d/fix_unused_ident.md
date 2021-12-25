# Properly deserialize unused identifiers in the CoreFn

  This mostly affects downstream consumers of the CoreFn as discussed in
  #4201. This makes it so CoreFn deserialization properly reads `$__unused`
  into `UnusedIdent` instead of an `Ident`. This is particularly useful for
  downstream consumers of the CoreFn such as alternative backends that don't
  allow arguments to be omitted from functions.

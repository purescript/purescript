* Keep inlined `Data.Int.Bits.zshr` results within the signed 32-bit `Int` range.

  Zero-fill right shifts of negative inputs by zero or a multiple of 32 now
  return the original signed input instead of an out-of-range unsigned value.
  Nonzero effective shift counts retain zero-fill semantics. Code relying on
  `zshr x 0` as an unsigned conversion will change behavior.

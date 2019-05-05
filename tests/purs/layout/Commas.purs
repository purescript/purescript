module Test where

test =
  [ case do foo, bar of
      a | b, c -> d, bar
    ]

test =
  [ case do foo, bar of a | b, c -> d, bar ]

test =
  [ do do do foo, bar ]

test =
  [ \foo -> foo, bar ]

test = foo where
  bar =
    case a, b of
      c, d | d == [case true, w of 1, a -> true, false ] -> d
      e, d | do what, do that -> d


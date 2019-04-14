module Test where

-- Including data because of `|` masking
data Foo
 = Foo
 | Bar
 | Baz

test =
  case foo of
    a | b, c ->
      d
    a | b, c -> d

test = case a, b of
  c, d
   | e ->
     case e of
       f | true -> bar
         | false -> baz
   | f -> g

test a
  | false =
      case false of
        true | a > 12 -> true
  | otherwise = true

test = case a of foo | foo \a -> a -> true

test = a `case _ of x | unit # \_ -> true, true -> const` b

test = case a of
  12 | do that
          that   -> this
     | otherwise -> this

test a b = [ case _ of
  12 | case a, b of
         _, 42 -> b
         _, 12 -> false, b -> true
     | case a, b of
         _, 42 -> b
         _, 12 -> false, b -> true, false ]

test a
  | case a, b of
         _, 42 -> b
         _, 12 -> false, b = true
  | case a, b of
         _, 42 -> b
         _, 12 -> false, b = true


module PatternScope where

  -- Pattern scope doesn't leak
  foobar = \x -> {
      var y = case x of
	z | z % 2 == 0 -> 1
	_ -> 2;
      return z;
    }


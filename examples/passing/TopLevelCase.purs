gcd :: (Number, Number) -> Number
gcd (0, x) = x
gcd (x, 0) = x
gcd (x, y) | x > y = gcd (x % y, y)
gcd (x, y) = gcd (y % x, x)

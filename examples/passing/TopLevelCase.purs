_1 :: (Number, Number) -> Number
_1 (0, x) = x
_1 (x, 0) = x
_1 (x, y) | x > y = _1 (x % y, y)
_1 (x, y) = _1 (y % x, x)

guardsTest (x:xs) | x > 0 = guardsTest xs
guardsTest xs = xs

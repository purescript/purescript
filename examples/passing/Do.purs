module Do where

data Maybe a = Nothing | Just a

bindMaybe :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just a) f = f a

maybe = { ret: Just, bind: bindMaybe }

test1 = maybe do Just "abc"

test2 = maybe do return "xyz"

test3 = maybe do
  (x : _) <- Just [1, 2, 3]
  (y : _) <- Just [4, 5, 6]
  return x + y

test4 = maybe do
  Just 1
  Nothing :: Maybe Number
  return 2

test5 mx my = maybe do
  x <- mx
  y <- my
  return x + y

test6 mx my mz = maybe do
  x <- mx
  y <- my
  let sum = x + y
  z <- mz
  return z + sum

test7 mx = maybe do let Just x = mx
                    return x

test8 = maybe do return (maybe do return 1)

(<$>) :: forall a b. (a -> b) -> Maybe a -> Maybe b
(<$>) f m = maybe do
  a <- m
  return f a

(<*>) :: forall a b. (Maybe (a -> b)) -> Maybe a -> Maybe b
(<*>) f m = maybe do
  g <- f
  a <- m
  return g a

foo x y z = x + y + z

test9 = foo <$> Just 1 <*> Just 2 <*> Just 3

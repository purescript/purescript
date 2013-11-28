foreign import data IO :: * -> *

foreign import ret :: forall a. a -> IO a

foreign import (>>=) :: forall a b. IO a -> (a -> IO b) -> IO b

(*>) = \a -> \b -> a >>= \x -> b

foreign import putStrLn :: String -> IO {}

replicateM_ :: forall a. Number -> IO a -> IO {}
replicateM_ = \n -> \x -> do
  var io = ret {}
  for i <- 0 until n:
    io = x *> io
  return io

main = replicateM_ 10 (putStrLn "Hello World!")


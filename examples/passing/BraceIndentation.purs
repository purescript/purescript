module Main where

f x = case x of { 0 -> true 
                ; 1 -> false
                ; n -> f (n - 2)
                }

test :: forall m a b. (Monad m) => m (a -> b) -> m a -> m b
test a b = do { f <- a; x <- b; return (f x) }

main = Debug.Trace.trace "Done"

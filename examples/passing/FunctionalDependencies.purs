module Main where

import Control.Monad.Eff.Console (log)

data Nil
data Cons x xs

class Append a b c | a b -> c

instance appendNil :: Append Nil b b

instance appendCons :: Append xs b c => Append (Cons x xs) b (Cons x c)

appendProxy :: forall a b c. Append a b c => @a -> @b -> @c
appendProxy _ _ = @c

test = appendProxy @(Cons Int Nil) @(Cons String Nil)

main = log "Done"

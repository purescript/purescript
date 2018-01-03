module Main where

import Control.Monad.Eff.Console (log)

data Nil
data Cons x xs

class Append a b c | a b -> c

instance appendNil :: Append Nil b b

instance appendCons :: Append xs b c => Append (Cons x xs) b (Cons x c)

data Proxy a = Proxy

appendProxy :: forall a b c. Append a b c => Proxy a -> Proxy b -> Proxy c
appendProxy Proxy Proxy = Proxy

test = appendProxy (Proxy :: Proxy (Cons Int Nil)) (Proxy :: Proxy (Cons String Nil))

main = log "Done"

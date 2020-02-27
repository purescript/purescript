module Main where

import Effect.Console (log)

data Proxy a = Proxy

class CtorKind ctor (kind :: Type) | ctor -> kind

instance ctorKind0 :: CtorKind f z => CtorKind (f a) z
else instance ctorKind1 :: CtorKind ((a) :: t) t

data Test a b

ctorKind :: forall t k. CtorKind t k => Proxy t -> Proxy k
ctorKind _ = Proxy

testCtor1 = ctorKind (Proxy :: Proxy (Test Int String))
testCtor2 = ctorKind (Proxy :: Proxy (Test Int "What"))
testCtor3 = ctorKind (Proxy :: Proxy (Test Int))

main = log "Done"

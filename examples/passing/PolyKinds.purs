module Main where

import Prelude (Unit)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

data Proxy a = Proxy

test1 :: Proxy Array
test1 = Proxy

test2 :: Proxy Object
test2 = Proxy

test3 :: Proxy Int
test3 = Proxy

liftProxy :: forall a. Proxy a -> Proxy (Proxy a)
liftProxy _ = Proxy

unsafeCoerce :: forall a b. a -> b
unsafeCoerce a = unsafeCoerce a

data Exists (f :: k -> *)

runExists :: forall f r. (forall a. f a -> r) -> Exists f -> r
runExists = unsafeCoerce

mkExists :: forall f a. f a -> Exists f
mkExists = unsafeCoerce

existsObject :: forall a. a -> Exists Object
existsObject _ = mkExists { polykinded: true }

existsArray :: forall a. a -> Exists Array
existsArray _ = mkExists [0]

test4 :: forall a. a -> Int
test4 a = runExists (\_ -> 0) (existsArray a)

-- | Mutually recursive polykinded types
data Foo a = Foo (Bar a)

data Bar a = Bar (Foo a)

test5 :: forall a b. b -> Foo a
test5 b = Foo bar
  where
  bar :: Bar a
  bar = Bar (test5 b)

type OnEmpty f = f ()

test6 :: OnEmpty Object
test6 = {}

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = log "Done"

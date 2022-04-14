module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertEqual)
import CustomAssert (assertThrows)

force :: forall a b. (Unit -> b) -> b
force f = f unit

alpha = { backref: \_ -> bravo, x: 1 }
bravo = force \_ -> alpha.x


complicatedIdentity :: forall a. a -> a
complicatedIdentity = h
  where
  -- This highly contrived function tests that escalating force is caught and
  -- doesn't cause an infinite loop during compilation. ("Escalating force"
  -- means that invoking `f` with two argument leads to `f` being invoked with
  -- three arguments, and so on.)

  -- If the escalating loop in `f` isn't taken into account, `h` might be
  -- initialized before `g`, which will lead to a run-time error. The intended
  -- behavior is to lazily initialize `g` and `h` together, and let the fact
  -- that at run time `g` never actually dereferences `h` resolve the
  -- initialization ordering.

  f :: forall a. Int -> { tick :: a -> a, tock :: a -> a }
  f n = { tick: if n <= 0 then identity else (f (n - 1)).tock identity, tock: \a -> g n a }

  g :: forall a. Int -> a -> a
  g = (\bit -> if bit then \n -> (f n).tick else const h) true

  h :: forall a. a -> a
  h = (\n -> (f n).tick) 10


foreign import runtimeImportImpl :: forall a. Maybe String -> (String -> Maybe String) -> String -> (Maybe String -> Effect a) -> Effect a

runtimeImport :: forall a. String -> (Maybe String -> Effect a) -> Effect a
runtimeImport = runtimeImportImpl Nothing Just

type ID = forall a. a -> a

main = do
  err <- assertThrows \_ ->
    let
      selfOwn = { a: 1, b: force \_ -> selfOwn.a }
    in selfOwn
  assertEqual { actual: err, expected: "ReferenceError: selfOwn was needed before it finished initializing (module Main, line 52)" }

  err2 <- assertThrows \_ ->
    let
      f = (\_ -> { left: g identity, right: h identity }) unit

      g :: ID -> ID
      g x = (j x x x).right

      h :: ID -> ID -> { left :: ID, right :: ID }
      h x = j x x

      j x y z = { left: x y z, right: f.left }
    in f
  assertEqual { actual: err2, expected: "ReferenceError: f was needed before it finished initializing (module Main, line 66)" }

  assertEqual { actual: bravo, expected: 1 }
  runtimeImport "InitializationError" \err3 -> do
    assertEqual { actual: err3, expected: Just "ReferenceError: alphaArray was needed before it finished initializing (module InitializationError, line 0)" } -- TODO: fix the 0
    log "Done"

module Control.Monad.Eff 
  ( Eff()
  , Pure()
  , runPure
  , untilE
  , whileE
  , forE
  , foreachE
  ) where

foreign import data Eff :: # ! -> * -> *

foreign import returnE """
  function returnE(a) {
    return function() {
      return a;
    };
  }""" :: forall e a. a -> Eff e a

foreign import bindE """
  function bindE(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  }""" :: forall e a b. Eff e a -> (a -> Eff e b) -> Eff e b

type Pure a = forall e. Eff e a

foreign import runPure """
  function runPure(f) {
    return f();
  }""" :: forall a. Pure a -> a

instance functorEff :: Functor (Eff e) where
  (<$>) = liftA1

instance applyEff :: Apply (Eff e) where
  (<*>) = ap

instance applicativeEff :: Applicative (Eff e) where
  pure = returnE

instance bindEff :: Bind (Eff e) where
  (>>=) = bindE

instance monadEff :: Monad (Eff e)

foreign import untilE """
  function untilE(f) {
    return function() {
      while (!f());
      return {};
    };
  }""" :: forall e. Eff e Boolean -> Eff e Unit

foreign import whileE """
  function whileE(f) {
    return function(a) {
      return function() {
        while (f()) {
          a();
        }
        return {};
      };
    };
  }""" :: forall e a. Eff e Boolean -> Eff e a -> Eff e Unit

foreign import forE """
  function forE(lo) {
    return function(hi) {
      return function(f) {
        return function() {
          for (var i = lo; i < hi; i++) {
            f(i)();
          }
        };
      };
    };
  }""" :: forall e. Number -> Number -> (Number -> Eff e Unit) -> Eff e Unit

foreign import foreachE """
  function foreachE(as) {
    return function(f) {
      return function() {
        for (var i = 0; i < as.length; i++) {
          f(as[i])();
        }
      };
    };
  }""" :: forall e a. [a] -> (a -> Eff e Unit) -> Eff e Unit
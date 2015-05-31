module Main where

  import Control.Monad.Eff

  fiblike :: Int -> String
  fiblike 0 = "0"
  fiblike 1 = "1"
  fiblike n = getParamName fiblike

  -- This test ensures that the rendered javascript function uses a "pretty"
  -- variable name taken from the purescript function binding: in this case "n"
  --
  main = do
    assertEqual (fiblike 10) "n"
    Debug.Trace.trace "Done"

  foreign import getParamName """
    function getParamName(func) {
      var fnStr = func.toString();
      fnStr = fnStr.slice(fnStr.indexOf('function'));
      return fnStr.slice(fnStr.indexOf('(') + 1,fnStr.indexOf(')'));
    }
  """ :: (Int -> String) -> String

  foreign import assertEqual """
    function assertEqual(a) {
      return function(b) {
        return function() {
          if (a != b) {
            throw new Error('Assertion failed!');
          }
        };
      };
    }
  """ :: forall eff a. a -> a -> Eff eff Unit

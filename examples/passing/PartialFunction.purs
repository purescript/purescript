module Main where

foreign import testError
  """
  function testError(f) {
    try {
      return f();
    } catch (e) {
      if (e instanceof Error) return 'success';
      throw new Error('Pattern match failure is not Error');
    }
  }
  """ :: (Unit -> Number) -> String

fn :: Number -> Number
fn 0 = 0
fn 1 = 2

main = Debug.Trace.trace (show $ testError $ \_ -> fn 2)

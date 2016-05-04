module Main where

import Prelude
import Data.Function.Uncurried
import Control.Monad.Eff
import Control.Monad.Eff.Console

f = mkFn2 $ \a b -> runFn2 g a b + runFn2 g b a

g = mkFn2 $ \a b -> case {} of
  _ | a <= 0.0 || b <= 0.0 -> b
  _ -> runFn2 f (a - 0.0) (b - 0.0)

main = do
  runFn0 (mkFn0 $ \_ -> log $ show 0.0)
  runFn1 (mkFn1 $ \a -> log $ show a) 0.0
  runFn2 (mkFn2 $ \a b -> log $ show [a, b]) 0.0 0.0
  runFn3 (mkFn3 $ \a b c -> log $ show [a, b, c]) 0.0 0.0 0.0
  runFn4 (mkFn4 $ \a b c d -> log $ show [a, b, c, d]) 0.0 0.0 0.0 0.0
  runFn5 (mkFn5 $ \a b c d e -> log $ show [a, b, c, d, e]) 0.0 0.0 0.0 0.0 0.0
  runFn6 (mkFn6 $ \a b c d e f -> log $ show [a, b, c, d, e, f]) 0.0 0.0 0.0 0.0 0.0 0.0
  runFn7 (mkFn7 $ \a b c d e f g -> log $ show [a, b, c, d, e, f, g]) 0.0 0.0 0.0 0.0 0.0 0.0 0.0
  runFn8 (mkFn8 $ \a b c d e f g h -> log $ show [a, b, c, d, e, f, g, h]) 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
  runFn9 (mkFn9 $ \a b c d e f g h i -> log $ show [a, b, c, d, e, f, g, h, i]) 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
  runFn10 (mkFn10 $ \a b c d e f g h i j-> log $ show [a, b, c, d, e, f, g, h, i, j]) 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
  logShow $ runFn2 g 0.0 0.0
  log "Done"

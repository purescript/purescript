module Main where

import Control.Monad.Eff.Console (log)

data T a = T

-- inferred as `Array t0`
xs :: _
xs = [T, T, T]

t :: forall a. T a
t = T

-- inferred as `Array (forall a. T a)`
xs' :: _
xs' = [t, t, t]

main = log "Done"

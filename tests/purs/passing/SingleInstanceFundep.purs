module Main where

import Prelude
import Effect.Console (log)
import Type.Proxy (Proxy(..))

-- This class can only have a single instance due to the functional dependency
class SingleInstanceFundep (r :: Row Type) | -> r where
  unified :: Proxy r

-- The row literal is valid in this instance head since it is fully determined
instance SingleInstanceFundep ( x :: Unit ) where
  unified = Proxy

-- This should infer `test :: Proxy ( x :: Unit )` by committing to the instance
test :: Proxy _
test = unified

main = do
  let (Proxy :: Proxy ( x :: Unit )) = test
  log "Done"

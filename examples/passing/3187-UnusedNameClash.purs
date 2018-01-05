module Main (main) where

import Prelude ((+))
import Control.Monad.Eff.Console (log)

-- the __unused parameter used to get optimized away
abuseUnused :: forall a. a -> a
abuseUnused __unused = __unused

main = do
  let explode = abuseUnused 0 + abuseUnused 0
  log "Done"

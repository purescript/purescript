module Main where

import Control.Monad.Eff.Console
 
-- '→' is multibyte sequence \u2192.
utf8multibyte = "Hello λ→ world!!"

main = do
  log "done"


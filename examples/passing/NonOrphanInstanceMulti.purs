-- Both l and r must be known, thus can be in separate modules
module Main where
import Control.Monad.Eff.Console (log)
import Lib
data L
instance clr :: C L R
main = log "Done"

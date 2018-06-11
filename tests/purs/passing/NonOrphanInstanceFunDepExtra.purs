-- Both f and l must be known, thus can be in separate modules
module Main where
import Effect.Console (log)
import Lib
data F
data R
instance cflr :: C F L R
main = log "Done"

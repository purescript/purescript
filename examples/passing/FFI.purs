module FFI where

foreign import foo :: String -> String

bar :: String -> String
bar _ = foo "test"

module FFIModuleTest where

import FFI

baz _ = foo "test"
    
module Main where

main = Trace.trace "Done"

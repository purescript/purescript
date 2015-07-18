module DoNotSuggestComposition where

import Prelude

x = { y: 3 }

foo :: String -> String
foo y = y

bar = foo x

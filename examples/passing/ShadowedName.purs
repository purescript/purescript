module Main where

import Prelude
import Control.Monad.Eff.Console
import Control.Monad.Eff.Console (log)

done :: String
done = let str = "Not yet done" in
        let str = "Done" in str

main = log done

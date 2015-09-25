module Main where

import Prelude
import Control.Monad.Eff.Console

done :: String
done = let str = "Not yet done" in
        let str = "Done" in str

main = Control.Monad.Eff.Console.log done

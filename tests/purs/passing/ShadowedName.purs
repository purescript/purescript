module Main where

import Prelude
import Effect.Console
import Effect.Console (log)

done :: String
done = let str = "Not yet done" in
        let str = "Done" in str

main = log done

module Main where

import Prelude
import Debug.Trace

done :: String
done = let str = "Not yet done" in
        let str = "Done" in str

main = Debug.Trace.trace done

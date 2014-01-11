module Tests where

import Prelude
import Eff
import Errors
import Trace
 
test n = runPure $ catchError (\s -> eff do return 0) $ eff do 
  case {} of 
    _ | n > 10 -> eff do
      --trace "n > 10"
      throwError "Error!" 
    _ -> eff do return n

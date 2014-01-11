module Tests where

import Prelude
import Errors

test n = catchError (\s -> eff do return 0) ( eff do 
  case {} of 
    _ | n > 10 -> throwError "Error!" 
    _ -> eff do return n)

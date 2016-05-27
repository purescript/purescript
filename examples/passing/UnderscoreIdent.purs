module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data Data_type = Con_Structor | Con_2 String

type Type_name = Data_type

done (Con_2 s) = s
done _ = "Failed"

main = log (done (Con_2 "Done"))

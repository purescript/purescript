module Main where

import Prelude

data Data_type = Con_Structor | Con_2 String

type Type_name = Data_type

done (Con_2 s) = s

main = Debug.Trace.trace (done (Con_2 "Done"))

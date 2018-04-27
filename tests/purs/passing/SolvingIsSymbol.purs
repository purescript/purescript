module Main where

import Prelude
import Effect
import Effect.Console

-- Here we import as alias of reflectSymbol without importing Data.Symbol. However,
-- Data.Symbol should be implicitly imported as we have an instance of IsSymbol solved.
import SolvingIsSymbol.Lib (literalSymbol, libReflectSymbol)

main = do
  let lit = libReflectSymbol literalSymbol
  when (lit == "literal") (log "Done")

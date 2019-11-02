module SolvingIsSymbol.Lib where

import Data.Symbol

literalSymbol :: SProxy "literal"
literalSymbol = SProxy

libReflectSymbol :: forall s. IsSymbol s => SProxy s -> String
libReflectSymbol = reflectSymbol


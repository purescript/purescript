module SolvingIsSymbol.Lib where

import Data.Symbol
import Type.Proxy (Proxy(..))

literalSymbol :: Proxy "literal"
literalSymbol = Proxy

libReflectSymbol :: forall s. IsSymbol s => Proxy s -> String
libReflectSymbol = reflectSymbol


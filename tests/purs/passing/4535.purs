module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Console (log)
import Type.Proxy (Proxy(..))

singleArgument :: forall @a. a -> Unit
singleArgument _ = unit

multiArgument :: forall @a @b. a -> b -> Unit
multiArgument _ _ = unit

singleApplication :: Int /\ Number -> Unit
singleApplication = singleArgument @(Int /\ Number)

-- Like expression applications, visible type applications are left-associative.
-- This test accounts for subsequent type applications nested in this manner.
appNestingWorks :: Int /\ Number -> Number /\ Int -> Unit
appNestingWorks = multiArgument @(Int /\ Number) @(Number /\ Int)

-- This test accounts for type applications nested within other AST nodes.
otherNestingWorks :: Array (Maybe (Int /\ Number))
otherNestingWorks = [Just @(Int /\ Number) (0 /\ 0.0), Just @(Int /\ Number) (1 /\ 1.0)]

type InSynonym = Int /\ Number

-- This test accounts for type synonyms used as type arguments.
-- Since expansion happens during checking, InSynonym would expand
-- to an already-desugared type operator. This test exists for the
-- sake of redundancy.
inSynonym :: InSynonym -> Unit
inSynonym = singleArgument @InSynonym

-- This test accounts for type operators used as type arguments directly.
operatorAsArgument :: Proxy (/\)
operatorAsArgument = Proxy @(/\)

main :: Effect Unit
main = log "Done"

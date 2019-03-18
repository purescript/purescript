-- A module for testing the :print feature for configuring the function used
-- for printing repl results
module InteractivePrint where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)

unsafeEval :: forall a. a -> Effect Unit
unsafeEval = log <<< unsafeCoerce

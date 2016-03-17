module Control.Monad.Eff.Console where

import Control.Monad.Eff (Eff)

import Data.Show (class Show, show)
import Data.Unit (Unit)

-- | The `CONSOLE` effect represents those computations which write to the
-- | console.
foreign import data CONSOLE :: !

-- | Write a message to the console.
foreign import log
  :: forall eff
   . String
  -> Eff (console :: CONSOLE | eff) Unit

-- | Write a value to the console, using its `Show` instance to produce a
-- | `String`.
logShow :: forall a eff. Show a => a -> Eff (console :: CONSOLE | eff) Unit
logShow a = log (show a)

-- | Write an warning to the console.
foreign import warn
  :: forall eff
   . String
  -> Eff (console :: CONSOLE | eff) Unit

-- | Write an warning value to the console, using its `Show` instance to produce
-- | a `String`.
warnShow :: forall a eff. Show a => a -> Eff (console :: CONSOLE | eff) Unit
warnShow a = warn (show a)

-- | Write an error to the console.
foreign import error
  :: forall eff
   . String
  -> Eff (console :: CONSOLE | eff) Unit

-- | Write an error value to the console, using its `Show` instance to produce a
-- | `String`.
errorShow :: forall a eff. Show a => a -> Eff (console :: CONSOLE | eff) Unit
errorShow a = error (show a)

-- | Write an info message to the console.
foreign import info
  :: forall eff
   . String
  -> Eff (console :: CONSOLE | eff) Unit

-- | Write an info value to the console, using its `Show` instance to produce a
-- | `String`.
infoShow :: forall a eff. Show a => a -> Eff (console :: CONSOLE | eff) Unit
infoShow a = info (show a)

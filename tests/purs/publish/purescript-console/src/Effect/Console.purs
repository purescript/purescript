module Effect.Console where

import Effect (Effect)

import Data.Show (class Show, show)
import Data.Unit (Unit)

-- | Write a message to the console.
foreign import log
  :: String
  -> Effect Unit

-- | Write a value to the console, using its `Show` instance to produce a
-- | `String`.
logShow :: forall a. Show a => a -> Effect Unit
logShow a = log (show a)

-- | Write an warning to the console.
foreign import warn
  :: String
  -> Effect Unit

-- | Write an warning value to the console, using its `Show` instance to produce
-- | a `String`.
warnShow :: forall a. Show a => a -> Effect Unit
warnShow a = warn (show a)

-- | Write an error to the console.
foreign import error
  :: String
  -> Effect Unit

-- | Write an error value to the console, using its `Show` instance to produce a
-- | `String`.
errorShow :: forall a. Show a => a -> Effect Unit
errorShow a = error (show a)

-- | Write an info message to the console.
foreign import info
  :: String
  -> Effect Unit

-- | Write an info value to the console, using its `Show` instance to produce a
-- | `String`.
infoShow :: forall a. Show a => a -> Effect Unit
infoShow a = info (show a)

-- | Start a named timer.
foreign import time :: String -> Effect Unit

-- | Stop a named timer and print time since it started in milliseconds.
foreign import timeEnd :: String -> Effect Unit

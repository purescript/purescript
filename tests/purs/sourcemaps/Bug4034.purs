-- | This module is the same as `purescript-effect@v6.0.0`'s `Effect.Console` file
-- | under a different module name.
-- | This verifies that null source spans are no longer emitted.
module SourceMaps.Bug4034 where

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

-- | Write an debug message to the console.
foreign import debug
  :: String
  -> Effect Unit

-- | Write an debug value to the console, using its `Show` instance to produce a
-- | `String`.
debugShow :: forall a. Show a => a -> Effect Unit
debugShow a = debug (show a)

-- | Start a named timer.
foreign import time :: String -> Effect Unit

-- | Print the time since a named timer started in milliseconds.
foreign import timeLog :: String -> Effect Unit

-- | Stop a named timer and print time since it started in milliseconds.
foreign import timeEnd :: String -> Effect Unit

-- | Clears the console
foreign import clear :: Effect Unit

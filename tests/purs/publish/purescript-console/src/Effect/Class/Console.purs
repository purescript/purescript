module Effect.Class.Console where

import Data.Function ((<<<))
import Data.Show (class Show)
import Data.Unit (Unit)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as EffConsole

log :: forall m. MonadEffect m => String -> m Unit
log = liftEffect <<< EffConsole.log

logShow :: forall m a. MonadEffect m => Show a => a -> m Unit
logShow = liftEffect <<< EffConsole.logShow

warn :: forall m. MonadEffect m => String -> m Unit
warn = liftEffect <<< EffConsole.warn

warnShow :: forall m a. MonadEffect m => Show a => a -> m Unit
warnShow = liftEffect <<< EffConsole.warnShow

error :: forall m. MonadEffect m => String -> m Unit
error = liftEffect <<< EffConsole.error

errorShow :: forall m a. MonadEffect m => Show a => a -> m Unit
errorShow = liftEffect <<< EffConsole.errorShow

info :: forall m. MonadEffect m => String -> m Unit
info = liftEffect <<< EffConsole.info

infoShow :: forall m a. MonadEffect m => Show a => a -> m Unit
infoShow = liftEffect <<< EffConsole.infoShow

time :: forall m. MonadEffect m => String -> m Unit
time = liftEffect <<< EffConsole.time

timeEnd :: forall m. MonadEffect m => String -> m Unit
timeEnd = liftEffect <<< EffConsole.timeEnd

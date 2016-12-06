{-# LANGUAGE PackageImports        #-}

module Language.PureScript.Ide.Logging
       ( runLogger
       , logPerf
       , displayTimeSpec
       ) where

import           Protolude

import           "monad-logger" Control.Monad.Logger
import qualified Data.Text as T
import           Language.PureScript.Ide.Types
import           System.Clock
import           Text.Printf

runLogger :: MonadIO m => IdeLogLevel -> LoggingT m a -> m a
runLogger logLevel' =
  runStdoutLoggingT . filterLogger (\_ logLevel ->
                                       case logLevel' of
                                         LogAll -> True
                                         LogDefault -> not (logLevel == LevelOther "perf" || logLevel == LevelDebug)
                                         LogNone -> False
                                         LogDebug -> not (logLevel == LevelOther "perf")
                                         LogPerf -> logLevel == LevelOther "perf")

logPerf :: (MonadIO m, MonadLogger m) => (TimeSpec -> Text) -> m t -> m t
logPerf format f = do
  start <- liftIO (getTime Monotonic)
  result <- f
  end <- liftIO (getTime Monotonic)
  logOtherN (LevelOther "perf") (format (diffTimeSpec start end))
  pure result

displayTimeSpec :: TimeSpec -> Text
displayTimeSpec ts =
  T.pack (printf "%0.2f" (fromIntegral (toNanoSecs ts) / 1000000 :: Double)) <> "ms"

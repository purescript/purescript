-- |
-- A replacement for WriterT IO which uses mutable references.
--
module Control.Monad.Logger where

import Prelude

import Control.Monad (ap)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Writer.Class (MonadWriter(..))

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)

-- | Logger monad, using IORef for mutable log accumulation.
data Logger w a
  = LoggerPure a
  | LoggerIO (IORef w -> IO a)

-- | Run a Logger computation given an existing IORef.
runLogger :: Logger w a -> IORef w -> IO a
runLogger (LoggerPure a) _ = return a
runLogger (LoggerIO f) r = f r

-- | Run a Logger computation, starting with an empty log.
runLogger' :: Monoid w => Logger w a -> IO (a, w)
runLogger' l = do
  ref <- newIORef mempty
  a <- runLogger l ref
  w <- readIORef ref
  return (a, w)

-- Functor
instance Functor (Logger w) where
  fmap f (LoggerPure a) = LoggerPure (f a)
  fmap f (LoggerIO m) = LoggerIO $ \r -> fmap f (m r)

-- Applicative
instance Monoid w => Applicative (Logger w) where
  pure = LoggerPure
  (<*>) = ap

-- Monad
instance Monoid w => Monad (Logger w) where
  return = pure
  LoggerPure a >>= f = f a
  LoggerIO m >>= f = LoggerIO $ \r -> do
    a <- m r
    runLogger (f a) r

-- MonadIO
instance Monoid w => MonadIO (Logger w) where
  liftIO = LoggerIO . const

-- MonadWriter
instance Monoid w => MonadWriter w (Logger w) where
  tell w = LoggerIO $ \r -> atomicModifyIORef' r $ \w' -> (mappend w' w, ())
  listen m = LoggerIO $ \r -> do
    (a, w) <- runLogger' m
    atomicModifyIORef' r $ \w' -> (mappend w' w, (a, w))
  pass m = LoggerIO $ \r -> do
    ((a, f), w) <- runLogger' m
    atomicModifyIORef' r $ \w' -> (mappend w' (f w), a)

-- MonadBase
instance Monoid w => MonadBase IO (Logger w) where
  liftBase = liftIO

-- MonadBaseControl
instance Monoid w => MonadBaseControl IO (Logger w) where
  type StM (Logger w) a = a
  liftBaseWith f = LoggerIO $ \r -> liftBaseWith $ \runInBase ->
    f (\m -> runInBase (runLogger m r))
  restoreM = return

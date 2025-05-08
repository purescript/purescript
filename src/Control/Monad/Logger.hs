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
import Language.PureScript.Errors (MultipleErrors (MultipleErrors))

-- | Logger monad, using IORef for mutable log accumulation.
data Logger a
  = LoggerPure a
  | LoggerIO (IORef MultipleErrors -> IO a)

-- | Run a Logger computation given an existing IORef.
runLogger :: Logger a -> IORef MultipleErrors -> IO a
runLogger (LoggerPure a) _ = return a
runLogger (LoggerIO f) r = f r

-- | Run a Logger computation, starting with an empty log.
runLogger' :: Logger a -> IO (a, MultipleErrors)
runLogger' l = do
  ref <- newIORef mempty
  a <- runLogger l ref
  (MultipleErrors list) <- readIORef ref
  return (a, MultipleErrors $ reverse list)

-- Functor
instance Functor Logger where
  fmap f (LoggerPure a) = LoggerPure (f a)
  fmap f (LoggerIO m) = LoggerIO $ \r -> fmap f (m r)

-- Applicative
instance Applicative Logger where
  pure = LoggerPure
  (<*>) = ap

-- Monad
instance Monad Logger where
  return = pure
  LoggerPure a >>= f = f a
  LoggerIO m >>= f = LoggerIO $ \r -> do
    a <- m r
    runLogger (f a) r

-- MonadIO
instance MonadIO Logger where
  liftIO = LoggerIO . const

-- MonadWriter
instance MonadWriter MultipleErrors Logger where
  tell w = LoggerIO $ \r ->
    atomicModifyIORef' r $ \(MultipleErrors acc) ->
      let MultipleErrors new = w
       in (MultipleErrors (new ++ acc), ())
  listen m = LoggerIO $ \r -> do
    (a, w) <- runLogger' m
    atomicModifyIORef' r $ \w' -> (mappend w' w, (a, w))
  pass m = LoggerIO $ \r -> do
    ((a, f), w) <- runLogger' m
    atomicModifyIORef' r $ \w' -> (mappend w' (f w), a)

-- MonadBase
instance MonadBase IO Logger where
  liftBase = liftIO

-- MonadBaseControl
instance MonadBaseControl IO Logger where
  type StM Logger a = a
  liftBaseWith f = LoggerIO $ \r -> liftBaseWith $ \runInBase ->
    f (\m -> runInBase (runLogger m r))
  restoreM = return

-----------------------------------------------------------------------------
--
-- Module      :  Control.Monad.Logger
-- Author      :  Phil Freeman
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | A replacement for WriterT IO which uses mutable references.
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Logger where

import Data.IORef

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
import Control.Applicative
#endif
import Control.Monad (ap)
import Control.Monad.IO.Class
import Control.Monad.Writer.Class
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))

-- | A replacement for WriterT IO which uses mutable references.
data Logger w a = Logger { runLogger :: IORef w -> IO a }

-- | Run a Logger computation, starting with an empty log.
runLogger' :: (Monoid w) => Logger w a -> IO (a, w)
runLogger' l = do
  r <- newIORef mempty
  a <- runLogger l r
  w <- readIORef r
  return (a, w)

instance Functor (Logger w) where
  fmap f (Logger l) = Logger $ \r -> fmap f (l r)

instance (Monoid w) => Applicative (Logger w) where
  pure = Logger . const . pure
  (<*>) = ap

instance (Monoid w) => Monad (Logger w) where
  return = pure
  Logger l >>= f = Logger $ \r -> l r >>= \a -> runLogger (f a) r

instance (Monoid w) => MonadIO (Logger w) where
  liftIO = Logger . const

instance (Monoid w) => MonadWriter w (Logger w) where
  tell w = Logger $ \r -> atomicModifyIORef' r $ \w' -> (mappend w' w, ())
  listen l = Logger $ \r -> do
    (a, w) <- liftIO (runLogger' l)
    atomicModifyIORef' r $ \w' -> (mappend w' w, (a, w))
  pass l = Logger $ \r -> do
    ((a, f), w) <- liftIO (runLogger' l)
    atomicModifyIORef' r $ \w' -> (mappend w' (f w), a)

instance (Monoid w) => MonadBase IO (Logger w) where
  liftBase = liftIO

instance (Monoid w) => MonadBaseControl IO (Logger w) where
  type StM (Logger w) a = a
  liftBaseWith f = Logger $ \r -> liftBaseWith $ \q -> f (q . flip runLogger r)
  restoreM = return

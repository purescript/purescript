module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Monoid
import Data.Tuple

type MyString = String 

newtype X = X MyString

derive newtype instance showX :: Show X
derive newtype instance eqX :: Eq X
derive newtype instance ordX :: Ord X

newtype Y a = Y (Array a)

derive newtype instance showY :: Show (Y String)

class Singleton a b where
  singleton :: a -> b

instance singletonArray :: Singleton a (Array a) where
  singleton x = [x]

derive newtype instance singletonY :: Singleton a (Y a)

newtype MyArray a = MyArray (Array a)

derive newtype instance showMyArray :: Show a => Show (MyArray a)
derive newtype instance functorMyArray :: Functor MyArray

newtype ProxyArray x a = ProxyArray (Array a)

derive newtype instance functorProxyArray :: Functor (ProxyArray x)

class (Monad m, Monoid w) <= MonadWriter w m | m -> w where
  tell :: w -> m Unit

instance monadWriterTuple :: Monoid w => MonadWriter w (Tuple w) where
  tell w = Tuple w unit

newtype MyWriter w a = MyWriter (Tuple w a)

derive newtype instance functorMyWriter :: Functor (MyWriter w)
derive newtype instance applyMyWriter :: Semigroup w => Apply (MyWriter w)
derive newtype instance applicativeMyWriter :: Monoid w => Applicative (MyWriter w)
derive newtype instance bindMyWriter :: Semigroup w => Bind (MyWriter w)
derive newtype instance monadMyWriter :: Monoid w => Monad (MyWriter w)
derive newtype instance monadWriterMyWriter :: Monoid w => MonadWriter w (MyWriter w)

main = do
  logShow (X "test")
  logShow (singleton "test" :: Y String)
  logShow (map show (MyArray [1, 2, 3]))
  log "Done"

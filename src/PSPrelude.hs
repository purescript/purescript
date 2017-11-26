module PSPrelude ( module X
                 , F.id
                 , L.groupBy
                 , Monad.fail
                 , error
                 , failT
                 , fatal
                 , putErrText
                 , putErrText'
                 , putErrTextLines
                 , putTextLines
                 , unsafeFromJust
                 , unsafeHead
                 , unsafeIndex
                 , unsafeInit
                 , unsafeLast
                 ) where

import Protolude as X hiding ( Associativity
                             , Constraint
                             , Constructor
                             , Coercion
                             , HasCallStack
                             , Fixity
                             , Infix
                             , Meta
                             , Proxy
                             , readFile
                             , writeFile
                             , check
                             , link
                             , moduleName
                             , replace
                             )

import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Function as F
import qualified Control.Monad as Monad

unsafeHead :: [a] -> a 
unsafeHead = L.head

unsafeIndex :: [a] -> Int -> a
unsafeIndex = (L.!!)

unsafeLast :: [a] -> a
unsafeLast = L.last

unsafeInit :: [a] -> [a]
unsafeInit = L.init

unsafeFromJust :: M.Maybe a -> a
unsafeFromJust = M.fromJust

error :: Text -> a
error = panic

putErrText :: MonadIO m => Text -> m ()
putErrText = X.hPutStrLn X.stderr

putErrText' :: MonadIO m => Text -> m ()
putErrText' = X.hPutStr X.stderr

putErrTextLines :: MonadIO m => [Text] -> m ()
putErrTextLines = putErrText' . T.unlines

putTextLines :: MonadIO m => [Text] -> m ()
putTextLines = X.putText . T.unlines

fatal :: Text -> IO a
fatal s = putErrText s >> exitFailure

failT :: Monad m => Text -> m a
failT = Monad.fail . toS

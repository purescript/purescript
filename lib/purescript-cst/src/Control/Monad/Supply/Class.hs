-- |
-- A class for monads supporting a supply of fresh names
--

module Control.Monad.Supply.Class where

import Prelude.Compat

import Control.Monad.Supply
import Control.Monad.State
import Control.Monad.Writer
import Data.Text (Text, pack)

class Monad m => MonadSupply m where
  -- |
  -- Get a new unique Integer
  fresh :: m Integer
  -- |
  -- See what the next unique Integer will be
  peek :: m Integer
  default fresh :: (MonadTrans t, MonadSupply n, m ~ t n) => m Integer
  fresh = lift fresh
  default peek :: (MonadTrans t, MonadSupply n, m ~ t n) => m Integer
  peek = lift peek

instance Monad m => MonadSupply (SupplyT m) where
  fresh = SupplyT $ do
    n <- get
    put (n + 1)
    return n
  peek = SupplyT get

instance MonadSupply m => MonadSupply (StateT s m)
instance (Monoid w, MonadSupply m) => MonadSupply (WriterT w m)

-- |
-- `fresh`, but formatted to be usable as the name of an identifier in JS code
-- generation.
--
-- At any point in the compilation pipeline prior to the renaming phase (which
-- takes place after CoreFn optimization and before converting to CoreImp and
-- optimizing that), if you want an Ident with a name that is guaranteed (by
-- the renamer) not to collide with an existing Ident, you should probably use
-- `Language.PureScript.Names.freshIdent` instead of this function.
--
freshName :: MonadSupply m => m Text
freshName = fmap (("$" <> ) . pack . show) fresh
